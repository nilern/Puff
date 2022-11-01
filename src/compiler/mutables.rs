use std::boxed;
use std::collections::hash_set::HashSet;
use std::collections::hash_map::HashMap;

use crate::compiler::anf::{self, PosExpr};
use crate::compiler::{Compiler, Id};
use crate::handle::{Handle, Root, root};
use crate::bool::Bool;

pub fn mutables(expr: &anf::PosExpr) -> HashSet<Id> {
    use anf::Expr::*;

    fn expr_mutables(mutables: &mut HashSet<Id>, expr: &anf::PosExpr) {
        match expr.expr {
            Define(_, ref val_expr) => expr_mutables(mutables, val_expr),
            GlobalSet(_, ref val_expr) => expr_mutables(mutables, val_expr),

            Begin(ref stmts) => for stmt in stmts { expr_mutables(mutables, stmt) },

            Let(ref bindings, ref body, _) | Letrec(ref bindings, ref body) => {
                for (_, val_expr) in bindings { expr_mutables(mutables, val_expr); }
                expr_mutables(mutables, body);
            },

            If(ref cond, ref conseq, ref alt, _) => {
                expr_mutables(mutables, cond);
                expr_mutables(mutables, conseq);
                expr_mutables(mutables, alt);
            },

            Set(id, ref val_expr) => {
                mutables.insert(id);
                expr_mutables(mutables, val_expr);
            },

            Fn(_, _, _, ref body) => expr_mutables(mutables, body),

            Call(ref cargs, _) =>
                for (_, val_expr) in cargs { expr_mutables(mutables, val_expr); },

            CallWithValues((_, ref producer), (_, ref consumer), _) => {
                expr_mutables(mutables, producer);
                expr_mutables(mutables, consumer);
            },

            Global(_) => (),
            Triv(_) => (),

            CheckedUse{..} | UninitializedBox
            | Box(..) | BoxSet(..) | CheckedBoxSet{..} | BoxGet(..) | CheckedBoxGet{..} => unreachable!()
        }
    }

    let mut mutables = HashSet::new();
    expr_mutables(&mut mutables, expr);
    mutables
}

// OPTIMIZE: Fixing Letrec (Reloaded)
pub fn letrec(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: anf::PosExpr) -> anf::PosExpr {
    use anf::Expr::*;
    use anf::Triv::*;

    enum Access {
        CheckedIndirect {guard: Id, r#box: Id},
        Checked {guard: Id},
        Indirect {r#box: Id},
        Direct
    }

    #[derive(Clone, Copy)]
    enum Init { Un, Statically, Semantically }

    #[derive(Clone, Copy)]
    struct IdState {
        guard: Id,
        init: Init,
        src_mutable: bool,
        used_guard: bool,
        r#box: Option<Id>,
    }

    struct Env(HashMap<Id, IdState>);

    impl Env {
        fn new() -> Self { Self(HashMap::new()) }

        fn insert(&mut self, guard: Id, id: Id, src_mutable: bool) {
            self.0.insert(id, IdState {
                guard,
                init: Init::Un,
                src_mutable,
                used_guard: false,
                r#box: if !src_mutable { None } else { Some(id) /* No separate immutable variable */ } });
        }

        fn initialize_statically(&mut self, id: Id) { self.0.get_mut(&id).unwrap().init = Init::Statically; }

        fn initialize_semantically(&mut self, id: Id) { self.0.get_mut(&id).unwrap().init = Init::Semantically; }

        fn get(&mut self, cmp: &mut Compiler, id: Id) -> Access {
            match self.0.get_mut(&id) {
                Some(&mut IdState {init: Init::Un, guard, ref mut used_guard, ref mut r#box, ..}) => {
                    let r#box = r#box.unwrap_or_else(|| {
                        let b = Id::freshen(cmp, id);
                        *r#box = Some(b);
                        b
                    });
                    *used_guard = true;
                    Access::CheckedIndirect {guard, r#box}
                },

                Some(&mut IdState {init: Init::Statically, guard, ref mut used_guard, ..}) => {
                    *used_guard = true;
                    Access::Checked {guard}
                },

                Some(&mut IdState {init: Init::Semantically, src_mutable, r#box, ..}) =>
                    if !src_mutable {
                        Access::Direct
                    } else {
                        Access::Indirect {r#box: r#box.unwrap()}
                    },

                None => Access::Direct // Nonrecursively bound
            }
        }

        fn get_box(&self, id: Id) -> Option<Id> {
            match self.0.get(&id) {
                Some(IdState{r#box, ..}) => *r#box,
                None => None
            }
        }

        fn post_get(&self, id: Id) -> IdState { *self.0.get(&id).unwrap() }
    }

    fn convert(cmp: &mut Compiler, mutables: &HashSet<Id>, env: &mut Env, expr: anf::PosExpr) -> anf::PosExpr {
        let PosExpr {expr, pos} = expr;
        
        let expr = match expr {
            Define(definiend, val_expr) =>
                Define(definiend, boxed::Box::new(convert(cmp, mutables, env, *val_expr))),

            GlobalSet(name, val_expr) =>
                GlobalSet(name, boxed::Box::new(convert(cmp, mutables, env, *val_expr))),

            Begin(stmts) => Begin(stmts.into_iter().map(|stmt| convert(cmp, mutables, env, stmt)).collect()),

            Let(bindings, body, live_outs) =>
                Let(bindings.into_iter().map(|(id, val_expr)| (id, convert(cmp, mutables, env, val_expr))).collect(),
                    boxed::Box::new(convert(cmp, mutables, env, *body)),
                    live_outs),

            // OPTIMIZE: If all `lambda`:s, emit `fix` instead:
            Letrec(bindings, body) => {
                let original_binders: Vec<(Id, Handle)> = bindings.iter()
                    .map(|(id, val_expr)| (*id, val_expr.pos.clone()))
                    .collect();

                let guard = Id::fresh(cmp);

                // Add id:s from `bindings` to env, guarded by `guard` when required:
                for (id, _) in original_binders.iter() {
                    env.insert(guard, *id, mutables.contains(id));
                }

                // Convert `bindings`:
                let converted_bindings = bindings.into_iter()
                    .map(|(id, val_expr)| {
                        let pos = val_expr.pos.clone();
                        let val_expr = convert(cmp, mutables, env, val_expr);
                        
                        env.initialize_statically(id);

                        match env.get_box(id) {
                            None => (id, val_expr),

                            Some(r#box) if r#box != id => { // `id` was not in `mutables`
                                let tmp = Id::freshen(cmp, id);
                                let tmp_use = PosExpr {expr: Triv(Use(tmp)), pos: pos.clone()};
                                (id, PosExpr {
                                    expr: Let(vec![(tmp, val_expr)],
                                        boxed::Box::new(PosExpr {expr: Begin(vec![
                                            PosExpr {expr: BoxSet(r#box, boxed::Box::new(tmp_use)), pos: pos.clone()},
                                            PosExpr {expr: Triv(Use(tmp)), pos: pos.clone()}
                                        ]), pos: pos.clone()}),
                                        anf::LiveVars::new()),
                                    pos
                                })
                            }

                            Some(_) => {
                                let ignore = Id::freshen(cmp, id);
                                (ignore, PosExpr {expr: BoxSet(id, boxed::Box::new(val_expr)), pos})
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                // `guard` not required after this point, according to `letrec` semantics:
                for (id, _) in original_binders.iter() {
                    env.initialize_semantically(*id);
                }

                // Convert body:
                let body_pos = body.pos.clone();
                let mut body = convert(cmp, mutables, env, *body);

                // Bindings for boxes if they were used:
                let mut final_bindings = Vec::new();
                let mut guard_used = false;
                for (id, pos) in original_binders {
                    let id_state = env.post_get(id);

                    if let Some(r#box) = id_state.r#box {
                        final_bindings.push((r#box, PosExpr {expr: UninitializedBox, pos}));
                    }

                    guard_used = guard_used || id_state.used_guard;
                }

                if guard_used {
                    // (guard (box #f))
                    let fals = PosExpr {
                        expr: Triv(Const(root!(&mut cmp.mt, Bool::instance(cmp.mt, false)).into())),
                        pos: pos.clone()
                    };
                    final_bindings.push((guard, PosExpr {expr: Box(boxed::Box::new(fals)), pos: pos.clone()}));

                    // (set! guard #t)
                    let tru = PosExpr {
                        expr: Triv(Const(root!(&mut cmp.mt, Bool::instance(cmp.mt, true)).into())),
                        pos: pos.clone()
                    };
                    body = PosExpr {
                        expr: Begin(vec![
                            PosExpr {expr: BoxSet(guard, boxed::Box::new(tru)), pos: body_pos.clone()},
                            body
                        ]),
                        pos: body_pos
                    }
                }

                final_bindings.extend(converted_bindings);
                Let(final_bindings, boxed::Box::new(body), anf::LiveVars::new())
            },

            If(cond, conseq, alt, live_outs) =>
                If(boxed::Box::new(convert(cmp, mutables, env, *cond)),
                    boxed::Box::new(convert(cmp, mutables, env, *conseq)),
                    boxed::Box::new(convert(cmp, mutables, env, *alt)),
                    live_outs),

            Set(id, val_expr) => {
                let val_expr = boxed::Box::new(convert(cmp, mutables, env, *val_expr));
                
                match env.get(cmp, id) {
                    Access::CheckedIndirect {guard, r#box} => {
                        debug_assert!(r#box == id);
                        CheckedBoxSet {guard, r#box, val_expr}
                    },
                    Access::Checked {guard} => CheckedBoxSet {guard, r#box: id, val_expr},
                    Access::Indirect {r#box} => BoxSet(r#box, val_expr),
                    Access::Direct => Set(id, val_expr)
                }
            }

            Fn(fvs, params, varargs, body) =>
                Fn(fvs, params, varargs, boxed::Box::new(convert(cmp, mutables, env, *body))),

            Call(cargs, live_outs) =>
                // Callee and args ids are not from source code and so cannot be recursively bound:
                Call(cargs.into_iter().map(|(id, val_expr)| (id, convert(cmp, mutables, env, val_expr))).collect(),
                    live_outs),

            CallWithValues((pid, producer), (cid, consumer), live_outs) =>
                CallWithValues((pid, boxed::Box::new(convert(cmp, mutables, env, *producer))),
                    (cid, boxed::Box::new(convert(cmp, mutables, env, *consumer))),
                    live_outs),

            Global(name) => Global(name),

            Triv(Use(id)) =>
                match env.get(cmp, id) {
                    Access::CheckedIndirect {guard, r#box} => CheckedBoxGet {guard, r#box},
                    Access::Checked {guard} => CheckedUse {guard, id},
                    Access::Indirect {r#box} => BoxGet(r#box),
                    Access::Direct => Triv(Use(id))
                },

            Triv(Const(c)) => Triv(Const(c)),

            CheckedUse{..}
            | Box(..) | UninitializedBox | BoxSet(..) | CheckedBoxSet{..} | BoxGet(..) | CheckedBoxGet{..} =>
                unreachable!()
        };

        PosExpr {expr, pos}
    }

    convert(cmp, mutables, &mut Env::new(), expr)
}


// TODO: Combine with `letrec` pass since that already emits boxes?
pub fn convert_mutables(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: anf::PosExpr) -> anf::PosExpr {
    use anf::Expr::*;
    use anf::Triv::*;

    let PosExpr {expr, pos} = expr;
    let expr = match expr {
        Define(definiend, val_expr) =>
            Define(definiend, boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))),

        GlobalSet(name, val_expr) =>
            GlobalSet(name, boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))),


        Begin(stmts) => Begin(stmts.into_iter().map(|stmt| convert_mutables(cmp, mutables, stmt)).collect()),

        Let(bindings, body, live_outs) => {
            let bindings = bindings.into_iter()
                .map(|(id, val_expr)| {
                    let val_expr = convert_mutables(cmp, mutables, val_expr);

                    if mutables.contains(&id) {
                        (id, PosExpr {expr: Box(boxed::Box::new(val_expr)), pos: pos.clone()})
                    } else {
                        (id, val_expr)
                    }
                })
                .collect();

            let body = convert_mutables(cmp, mutables, *body);

            Let(bindings, boxed::Box::new(body), live_outs)
        },

        If(cond, conseq, alt, live_outs) =>
            If(boxed::Box::new(convert_mutables(cmp, mutables, *cond)),
                boxed::Box::new(convert_mutables(cmp, mutables, *conseq)),
                boxed::Box::new(convert_mutables(cmp, mutables, *alt)),
                live_outs),

        Set(id, val_expr) => BoxSet(id, boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))),

        Box(val_expr) => Box(boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))),

        UninitializedBox => UninitializedBox,

        BoxSet(r#box, val_expr) => BoxSet(r#box, boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))),

        CheckedBoxSet {guard, r#box, val_expr} =>
            CheckedBoxSet {guard, r#box, val_expr: boxed::Box::new(convert_mutables(cmp, mutables, *val_expr))},

        BoxGet(r#box) => BoxGet(r#box),

        CheckedBoxGet {guard, r#box} => CheckedBoxGet {guard, r#box},

        Fn(fvs, params, varargs, body) => {
            let mut bindings = Vec::new();

            let params = params.into_iter()
                .map(|id|
                    if mutables.contains(&id) {
                        let new_id = Id::freshen(cmp, id);
                        bindings.push((id, PosExpr {
                            expr: Box(boxed::Box::new(PosExpr {expr: Triv(Use(new_id)), pos: pos.clone()})),
                            pos: pos.clone()
                        }));
                        new_id
                    } else {
                        id
                    }
                )
                .collect();

            let body = convert_mutables(cmp, mutables, *body);

            Fn(fvs, params, varargs, boxed::Box::new(
                if bindings.len() == 0 {
                    body
                } else {
                    PosExpr {expr: Let(bindings, boxed::Box::new(body), anf::LiveVars::new()), pos: pos.clone()}
                }))
        },

        Call(cargs, live_outs) =>
            // Callee and args ids are not from source code and so cannot be mutable:
            Call(cargs.into_iter().map(|(id, val_expr)| (id, convert_mutables(cmp, mutables, val_expr))).collect(),
                live_outs),

        CallWithValues((pid, producer), (cid, consumer), live_outs) =>
            CallWithValues((pid, boxed::Box::new(convert_mutables(cmp, mutables, *producer))),
                (cid, boxed::Box::new(convert_mutables(cmp, mutables, *consumer))),
                live_outs),

        Global(name) => Global(name),

        CheckedUse {guard, id} =>
            if mutables.contains(&id) {
                CheckedBoxGet {guard, r#box: id}
            } else {
                CheckedUse {guard, id}
            },

        Triv(Use(id)) => if mutables.contains(&id) { BoxGet(id) } else { Triv(Use(id)) },

        Triv(Const(c)) => Triv(Const(c)),

        Letrec(..) => unreachable!()
    };

    PosExpr {pos, expr}
}
