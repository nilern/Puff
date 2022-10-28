use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::io::stdout;
use std::boxed;
use pretty::RcDoc;

mod anf;
mod analyzer;
pub mod cfg;
mod to_cfg;
mod emit;

use crate::bytecode::Bytecode;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use analyzer::analyze;
use emit::emit;
use crate::bool::Bool;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl Id {
    pub fn src_fresh(cmp: &mut Compiler, name: HandleT<Symbol>) -> Self {
        let id = Self::fresh(cmp);
        cmp.names.insert(id, name);
        id
    }

    pub fn fresh(cmp: &mut Compiler) -> Self {
        let i = cmp.name_counter;
        cmp.name_counter = i + 1;
        Self(i)
    }

    pub fn freshen(cmp: &mut Compiler, old_id: Self) -> Self {
        let id = Self::fresh(cmp);

        if let Some(name) = cmp.names.get(&old_id) {
            cmp.names.insert(id, name.clone());
        }

        id
    }

    pub fn to_doc<'a>(self, cmp: &Compiler) -> RcDoc<'a, ()> {
        RcDoc::text(match cmp.names.get(&self) {
            Some(sym) => unsafe { format!("{}${}", sym.as_ref().name(), self.0) },
            None => format!("${}", self.0)
        })
    }
}

pub struct Compiler<'a> {
    pub mt: &'a mut Mutator,
    name_counter: usize,
    names: HashMap<Id, HandleT<Symbol>>
}

impl<'a> Compiler<'a> {
    fn new(mt: &'a mut Mutator) -> Self {
        Self {
            mt,
            name_counter: 0,
            names: HashMap::new()
        }
    }
}

pub fn compile(mt: &mut Mutator, expr: ORef, debug: bool) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let mut anf = analyze(&mut cmp, expr);
    if debug {
        anf.to_doc(cmp.mt, &cmp).render(80, &mut stdout());
        println!("");
    }

    let muts = mutables(&anf);

    anf = letrec(&mut cmp, &muts, &anf);

    anf = convert_mutables(&mut cmp, &muts, &anf); // `muts` should not have been invalidated by `letrec`

    liveness(&mut anf);

    let cfg = cfg::Fn::from(&anf);
    if debug {
        println!("{}", cfg.within(cmp.mt));
    }
    
    emit(&mut cmp, &cfg)
}

fn mutables(expr: &anf::Expr) -> HashSet<Id> {
    use anf::Expr::*;

    fn expr_mutables(mutables: &mut HashSet<Id>, expr: &anf::Expr) {
        match expr {
            &Define(_, ref val_expr) => expr_mutables(mutables, val_expr),
            &GlobalSet(_, ref val_expr) => expr_mutables(mutables, val_expr),

            &Begin(ref stmts) => for stmt in stmts { expr_mutables(mutables, stmt) },

            &Let(ref bindings, ref body, _) | &Letrec(ref bindings, ref body) => {
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

            &Call(_, _, _) => (),

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
fn letrec(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: &anf::Expr) -> anf::Expr {
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

            &Let(ref bindings, ref body, popnnt) =>
                Let(bindings.iter().map(|(id, val_expr)| (*id, convert(cmp, mutables, env, val_expr))).collect(),
                    boxed::Box::new(convert(cmp, mutables, env, body)),
                    popnnt),

            // OPTIMIZE: If all `lambda`:s, emit `fix` instead:
            &Letrec(ref bindings, ref body) => {
                let guard = Id::fresh(cmp);

                for (id, _) in bindings {
                    env.insert(guard, *id, mutables.contains(id));
                }

                let bindings = bindings.iter()
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
                                    ])),
                                    true))
                            }

                            Some(_) => {
                                let ignore = Id::freshen(cmp, *id);
                                (ignore, BoxSet(*id, boxed::Box::new(val_expr)))
                            }
                        }
                    })
                    .collect::<Vec<_>>();

                for (id, _) in bindings.iter() {
                    env.initialize_semantically(*id);
                }

                let mut body = convert(cmp, mutables, env, body);

                let mut impl_bindings = Vec::new();
                let mut guard_used = false;
                for (id, _) in bindings.iter() {
                    let id_state = env.post_get(*id);

                    if let Some(r#box) = id_state.r#box {
                        impl_bindings.push((r#box, UninitializedBox));
                    }

                    guard_used = id_state.used_guard;
                }
                if guard_used {
                    impl_bindings.push((guard,
                        Box(boxed::Box::new(Triv(Const(cmp.mt.root(Bool::instance(cmp.mt, false).into())))))));
                    body = Begin(vec![
                        BoxSet(guard, boxed::Box::new(Triv(Const(cmp.mt.root(Bool::instance(cmp.mt, true).into()))))),
                        body
                    ]);
                }

                impl_bindings.extend(bindings);
                Let(impl_bindings, boxed::Box::new(body), true)
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

            &Call(callee, ref args, ref live_outs) =>
                // Callee and args ids are not from source code and so cannot be recursively bound:
                Call(callee, args.clone(), live_outs.clone()),

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
fn convert_mutables(cmp: &mut Compiler, mutables: &HashSet<Id>, expr: &anf::Expr) -> anf::Expr {
    use anf::Expr::*;
    use anf::Triv::*;

    match expr {
        &Define(ref definiend, ref val_expr) =>
            Define(definiend.clone(), boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),

        &GlobalSet(ref name, ref val_expr) =>
            GlobalSet(name.clone(), boxed::Box::new(convert_mutables(cmp, mutables, val_expr))),


        &Begin(ref stmts) => Begin(stmts.iter().map(|stmt| convert_mutables(cmp, mutables, stmt)).collect()),

        &Let(ref bindings, ref body, popnnt) => {
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

            Let(bindings, boxed::Box::new(body), popnnt)
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
                    Let(bindings, boxed::Box::new(body), true)
                }))
        },

        &Call(callee, ref args, ref live_outs) =>
            // Callee and args ids are not from source code and so cannot be mutable:
            Call(callee, args.clone(), live_outs.clone()),

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

fn liveness(expr: &mut anf::Expr) {
    use anf::Expr::{self, *};
    use anf::Triv::*;

    fn live_ins(expr: &mut Expr, mut live_outs: anf::LiveVars) -> anf::LiveVars {
        match expr {
            &mut Define(_, ref mut val_expr) => live_ins(val_expr, live_outs),
            &mut GlobalSet(_, ref mut val_expr) => live_ins(val_expr, live_outs),

            &mut Begin(ref mut stmts) =>
                stmts.iter_mut().rev()
                    .fold(live_outs, |live_outs, stmt| live_ins(stmt, live_outs)),

            &mut Let(ref mut bindings, ref mut body, _) => {
                let body_live_ins = live_ins(body, live_outs);

                bindings.iter_mut().rev()
                    .fold(body_live_ins, |mut live_outs, (id, val_expr)| {
                        live_outs.remove(id);
                        live_ins(val_expr, live_outs)
                    })
            },

            &mut If(ref mut cond, ref mut conseq, ref mut alt, ref mut if_live_outs) => {
                *if_live_outs = live_outs.iter().copied().collect();

                let conseq_outs = live_outs.clone();
                let alt_ins = live_ins(alt, live_outs);
                let mut conseq_ins = live_ins(conseq, conseq_outs);

                conseq_ins.extend(alt_ins);
                live_ins(cond, conseq_ins)
            },

            &mut Box(ref mut val_expr) => live_ins(val_expr, live_outs),

            &mut UninitializedBox => live_outs,

            &mut BoxSet(r#box, ref mut val_expr) => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins
            },

            &mut CheckedBoxSet {guard, r#box, ref mut val_expr} => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins.insert(guard);
                val_expr_live_ins
            },

            &mut BoxGet(r#box) => { live_outs.insert(r#box); live_outs }

            &mut CheckedBoxGet {guard, r#box} => {
                live_outs.insert(r#box);
                live_outs.insert(guard);
                live_outs
            }

            &mut r#Fn(ref mut fvs, ref params, _, ref mut body) => {
                let mut free_vars = {
                    let mut live_outs = anf::LiveVars::new();
                    live_outs.insert(params[0]); // "self" closure should always be live
                    live_ins(body, live_outs)
                };

                for param in params {
                    free_vars.remove(param);
                }

                *fvs = free_vars.iter().copied().collect();

                live_outs.extend(free_vars);
                live_outs
            },

            &mut Call(callee, ref args, ref mut call_live_outs) => {
                *call_live_outs = live_outs.iter().copied().collect();

                for &arg in args.iter().rev() {
                    live_outs.insert(arg);
                }

                live_outs.insert(callee);
                live_outs
            },

            &mut Global(_) => live_outs,

            &mut CheckedUse {guard, id} => {
                live_outs.insert(id);
                live_outs.insert(guard);
                live_outs
            },

            &mut Triv(Use(id)) => { live_outs.insert(id); live_outs },

            &mut Triv(Const(_)) => live_outs,

            &mut Letrec(..) | &mut Set(..) => unreachable!()
        }
    }

    live_ins(expr, anf::LiveVars::new());
}
