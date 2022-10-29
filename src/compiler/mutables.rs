use std::boxed;
use std::collections::hash_set::HashSet;
use std::collections::hash_map::HashMap;

use crate::compiler::anf;
use crate::compiler::{Compiler, Id};
use crate::bool::Bool;

pub fn mutables(expr: &anf::Expr) -> HashSet<Id> {
    use anf::Expr::*;

    fn expr_mutables(mutables: &mut HashSet<Id>, expr: &anf::Expr) {
        match expr {
            &Define(_, ref val_expr) => expr_mutables(mutables, val_expr),
            &GlobalSet(_, ref val_expr) => expr_mutables(mutables, val_expr),

            &Begin(ref stmts) => for stmt in stmts { expr_mutables(mutables, stmt) },

            &Let(ref bindings, ref body) | &Letrec(ref bindings, ref body) => {
                for (_, val_expr) in bindings { expr_mutables(mutables, val_expr); }
                expr_mutables(mutables, body);
            },

            &If(ref cond, ref conseq, ref alt, _) => {
                expr_mutables(mutables, cond);
                expr_mutables(mutables, conseq);
                expr_mutables(mutables, alt);
            },

            &Set(id, ref val_expr) => {
                mutables.insert(id);
                expr_mutables(mutables, val_expr);
            },

            &Fn(_, _, _, ref body) => expr_mutables(mutables, body),

            &Call(ref cargs, _) =>
                for (_, val_expr) in cargs { expr_mutables(mutables, val_expr); }

            &Global(_) => (),
            &Triv(_) => (),

            &CheckedUse{..} | &UninitializedBox
            | &Box(..) | &BoxSet(..) | &CheckedBoxSet{..} | &BoxGet(..) | &CheckedBoxGet{..} => unreachable!()
        }
    }

    let mut mutables = HashSet::new();
    expr_mutables(&mut mutables, expr);
    mutables
}

// OPTIMIZE: Fixing Letrec (Reloaded)
pub fn letrec(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: &anf::Expr) -> anf::Expr {
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

    fn convert(cmp: &mut Compiler, mutables: &HashSet<Id>, env: &mut Env, expr: &anf::Expr) -> anf::Expr {
        match expr {
            &Define(ref definiend, ref val_expr) =>
                Define(definiend.clone(), boxed::Box::new(convert(cmp, mutables, env, val_expr))),

            &GlobalSet(ref name, ref val_expr) =>
                GlobalSet(name.clone(), boxed::Box::new(convert(cmp, mutables, env, val_expr))),

            &Begin(ref stmts) => Begin(stmts.iter().map(|stmt| convert(cmp, mutables, env, stmt)).collect()),

            &Let(ref bindings, ref body) =>
                Let(bindings.iter().map(|(id, val_expr)| (*id, convert(cmp, mutables, env, val_expr))).collect(),
                    boxed::Box::new(convert(cmp, mutables, env, body))),

            // OPTIMIZE: If all `lambda`:s, emit `fix` instead:
            &Letrec(ref bindings, ref body) => {
                let guard = Id::fresh(cmp);

                // Add id:s from `bindings` to env, guarded by `guard` when required:
                for (id, _) in bindings {
                    env.insert(guard, *id, mutables.contains(id));
                }

                // Convert `bindings`:
                let converted_bindings = bindings.iter()
                    .map(|(id, val_expr)| {
                        let val_expr = convert(cmp, mutables, env, val_expr);
                        
                        env.initialize_statically(*id);

                        match env.get_box(*id) {
                            None => (*id, val_expr),

                            Some(r#box) if r#box != *id => { // `id` was not in `mutables`
                                let tmp = Id::freshen(cmp, *id);
                                (*id, Let(vec![(tmp, val_expr)],
                                    boxed::Box::new(Begin(vec![
                                        BoxSet(r#box, boxed::Box::new(Triv(Use(tmp)))),
                                        Triv(Use(tmp))
                                    ]))))
                            }

                            Some(_) => {
                                let ignore = Id::freshen(cmp, *id);
                                (ignore, BoxSet(*id, boxed::Box::new(val_expr)))
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                // `guard` not required after this point, according to `letrec` semantics:
                for (id, _) in bindings.iter() {
                    env.initialize_semantically(*id);
                }

                // Convert body:
                let mut body = convert(cmp, mutables, env, body);

                // Bindings for boxes if they were used:
                let mut final_bindings = Vec::new();
                let mut guard_used = false;
                for (id, _) in bindings.iter() {
                    let id_state = env.post_get(*id);

                    if let Some(r#box) = id_state.r#box {
                        final_bindings.push((r#box, UninitializedBox));
                    }

                    guard_used = guard_used || id_state.used_guard;
                }

                if guard_used {
                    // (guard (box #f))
                    final_bindings.push((guard,
                        Box(boxed::Box::new(Triv(Const(cmp.mt.root(Bool::instance(cmp.mt, false).into())))))));

                    // (set! guard #t)
                    body = Begin(vec![
                        BoxSet(guard, boxed::Box::new(Triv(Const(cmp.mt.root(Bool::instance(cmp.mt, true).into()))))),
                        body
                    ]);
                }

                final_bindings.extend(converted_bindings);
                Let(final_bindings, boxed::Box::new(body))
            },

            &If(ref cond, ref conseq, ref alt, ref live_outs) =>
                If(boxed::Box::new(convert(cmp, mutables, env, cond)),
                    boxed::Box::new(convert(cmp, mutables, env, conseq)),
                    boxed::Box::new(convert(cmp, mutables, env, alt)),
                    live_outs.clone()),

            &Set(id, ref val_expr) => {
                let val_expr = boxed::Box::new(convert(cmp, mutables, env, val_expr));
                
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

            &Fn(ref fvs, ref params, varargs, ref body) =>
                Fn(fvs.clone(), params.clone(), varargs, boxed::Box::new(convert(cmp, mutables, env, body))),

            &Call(ref cargs, ref live_outs) =>
                // Callee and args ids are not from source code and so cannot be recursively bound:
                Call(cargs.iter().map(|(id, val_expr)| (*id, convert(cmp, mutables, env, val_expr))).collect(),
                    live_outs.clone()),

            &Global(ref name) => Global(name.clone()),

            &Triv(Use(id)) =>
                match env.get(cmp, id) {
                    Access::CheckedIndirect {guard, r#box} => CheckedBoxGet {guard, r#box},
                    Access::Checked {guard} => CheckedUse {guard, id},
                    Access::Indirect {r#box} => BoxGet(r#box),
                    Access::Direct => Triv(Use(id))
                },

            &Triv(Const(ref c)) => Triv(Const(c.clone())),

            &CheckedUse{..}
            | &Box(..) | &UninitializedBox | &BoxSet(..) | &CheckedBoxSet{..} | &BoxGet(..) | &CheckedBoxGet{..} =>
                unreachable!()
        }
    }

    convert(cmp, mutables, &mut Env::new(), expr)
}


// TODO: Combine with `letrec` pass since that already emits boxes?
pub fn convert_mutables(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: &anf::Expr) -> anf::Expr {
    use anf::Expr::*;
    use anf::Triv::*;

    match expr {
        &Define(ref definiend, ref val_expr) =>
            Define(definiend.clone(), boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),

        &GlobalSet(ref name, ref val_expr) =>
            GlobalSet(name.clone(), boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),


        &Begin(ref stmts) => Begin(stmts.iter().map(|stmt| convert_mutables(cmp, mutables, stmt)).collect()),

        &Let(ref bindings, ref body) => {
            let bindings = bindings.iter()
                .map(|(id, val_expr)| {
                    let val_expr = convert_mutables(cmp, mutables, val_expr);

                    if mutables.contains(id) {
                        (*id, Box(boxed::Box::new(val_expr)))
                    } else {
                        (*id, val_expr)
                    }
                })
                .collect();

            let body = convert_mutables(cmp, mutables, body);

            Let(bindings, boxed::Box::new(body))
        },

        &If(ref cond, ref conseq, ref alt, ref live_outs) =>
            If(boxed::Box::new(convert_mutables(cmp, mutables, cond)),
                boxed::Box::new(convert_mutables(cmp, mutables, conseq)),
                boxed::Box::new(convert_mutables(cmp, mutables, alt)),
                live_outs.clone()),

        &Set(id, ref val_expr) => BoxSet(id, boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),

        &Box(ref val_expr) => Box(boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),

        &UninitializedBox => UninitializedBox,

        &BoxSet(r#box, ref val_expr) => BoxSet(r#box, boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),

        &CheckedBoxSet {guard, r#box, ref val_expr} =>
            CheckedBoxSet {guard, r#box, val_expr: boxed::Box::new(convert_mutables(cmp, mutables, val_expr))},

        &BoxGet(r#box) => BoxGet(r#box),

        &CheckedBoxGet {guard, r#box} => CheckedBoxGet {guard, r#box},

        &Fn(ref fvs, ref params, varargs, ref body) => {
            let mut bindings = Vec::new();

            let params = params.iter()
                .map(|id|
                    if mutables.contains(id) {
                        let new_id = Id::freshen(cmp, *id);
                        bindings.push((*id, Box(boxed::Box::new(Triv(Use(new_id))))));
                        new_id
                    } else {
                        *id
                    }
                )
                .collect();

            let body = convert_mutables(cmp, mutables, body);

            Fn(fvs.clone(), params, varargs, boxed::Box::new(
                if bindings.len() == 0 {
                    body
                } else {
                    Let(bindings, boxed::Box::new(body))
                }))
        },

        &Call(ref cargs, ref live_outs) =>
            // Callee and args ids are not from source code and so cannot be mutable:
            Call(cargs.iter().map(|(id, val_expr)| (*id, convert_mutables(cmp, mutables, val_expr))).collect(),
                live_outs.clone()),

        &Global(ref name) => Global(name.clone()),

        &CheckedUse {guard, id} =>
            if mutables.contains(&id) {
                CheckedBoxGet {guard, r#box: id}
            } else {
                CheckedUse {guard, id}
            },

        &Triv(Use(id)) => if mutables.contains(&id) { BoxGet(id) } else { Triv(Use(id)) },

        &Triv(Const(ref c)) => Triv(Const(c.clone())),

        &Letrec(..) => unreachable!()
    }
}
