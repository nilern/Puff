use std::rc::Rc;
use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::anf;
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};
use crate::bool::Bool;

pub fn analyze(cmp: &mut Compiler, expr: ORef) -> anf::Expr {
    use anf::Expr::*;
    use anf::Triv::*;

    enum Env {
        Binding(Id, HandleT<Symbol>, Rc<Env>),
        Empty
    }

    impl Env {
        fn get(env: &Rc<Env>, name: Gc<Symbol>) -> Option<Id> {
            let mut env = env.clone();
            loop {
                match &*env {
                    &Self::Binding(id, ref bname, ref parent) =>
                        if **bname == name {
                            return Some(id);
                        } else {
                            env = parent.clone();
                        },
                    &Self::Empty => return None
                }
            }
        }
    }

    fn analyze_expr(cmp: &mut Compiler, env: &Rc<Env>, expr: ORef) -> anf::Expr {
        if let Ok(expr) = Gc::<()>::try_from(expr.clone()) {
            if let Some(name) = expr.try_cast::<Symbol>(cmp.mt) {
                if let Some(id) = Env::get(env, name) {
                    return Triv(Use(id));
                } else {
                    return Global(cmp.mt.root_t(name));
                }
            } else if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
                let callee = unsafe { ls.as_ref().car };
                if let Ok(callee) = Gc::<()>::try_from(callee) {
                    if let Some(callee) = callee.try_cast::<Symbol>(cmp.mt) {
                        if unsafe { callee.as_ref().name() == "let*" } {
                            return analyze_let(cmp, env, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "if" } {
                            return analyze_if(cmp, env, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "lambda" } {
                            return analyze_lambda(cmp, env, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "begin" } {
                            return analyze_begin(cmp, env, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "letrec" } {
                            return analyze_letrec(cmp, env, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "quote" } {
                            return analyze_quote(cmp, unsafe { ls.as_ref().cdr });
                        } else if unsafe { callee.as_ref().name() == "set!" } {
                            return analyze_set(cmp, env, unsafe { ls.as_ref().cdr });
                        }
                    }
                }

                return analyze_call(cmp, env, ls);
            }
        }

        Triv(Const(cmp.mt.root(expr)))
    }

    fn analyze_let(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        fn analyze_bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef) -> (Rc<Env>, Vec<anf::Binding>) {
            fn analyze_binding(cmp: &mut Compiler, env: &Rc<Env>, anf_bs: &mut Vec<anf::Binding>, binding: ORef)
                -> Rc<Env>
            {

                if let Some(binding) = binding.try_cast::<Pair>(cmp.mt) {
                    let pat = unsafe { binding.as_ref().car };

                    if let Some(binding) = unsafe { binding.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                        let val = unsafe { binding.as_ref().car };

                        if unsafe { binding.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                            if let Some(name) = pat.try_cast::<Symbol>(cmp.mt) {
                                let id = Id::fresh(cmp);
                                anf_bs.push((id, analyze_expr(cmp, &env, val)));
                                Rc::new(Env::Binding(id, cmp.mt.root_t(name), env.clone()))
                            } else {
                                todo!()
                            }
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }

            let mut anf_bs = Vec::new();
            let mut env = env.clone();

            let mut bs = bindings;
            while let Some(bs_pair) = bs.try_cast::<Pair>(cmp.mt) {
                env = analyze_binding(cmp, &env, &mut anf_bs, unsafe { bs_pair.as_ref().car });

                bs = unsafe { bs_pair.as_ref().cdr };
            }

            if bs == EmptyList::instance(cmp.mt).into() {
                (env, anf_bs)
            } else {
                todo!()
            }
        }

        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            let bindings = unsafe { args.as_ref().car };

            if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                let body = unsafe { args.as_ref().car };

                if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                    let (env, bindings) = analyze_bindings(cmp, env, bindings);
                    let body = analyze_expr(cmp, &env, body);
                    Let(bindings, boxed::Box::new(body), true)
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_letrec(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        fn bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef) -> (Rc<Env>, Vec<(Id, Gc<Symbol>, ORef)>) {
            fn binding(cmp: &mut Compiler, env: &Rc<Env>, binding: ORef) -> (Rc<Env>, (Id, Gc<Symbol>, ORef)) {
                if let Some(binding) = binding.try_cast::<Pair>(cmp.mt) {
                    let pat = unsafe { binding.as_ref().car };

                    if let Some(binding) = unsafe { binding.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                        let val = unsafe { binding.as_ref().car };

                        if unsafe { binding.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                            if let Some(name) = pat.try_cast::<Symbol>(cmp.mt) {
                                let id = Id::fresh(cmp);
                                (Rc::new(Env::Binding(id, cmp.mt.root_t(name), env.clone())), (id, name, val))
                            } else {
                                todo!()
                            }
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            }

            let mut anf_bs = Vec::new();
            let mut env = env.clone();

            let mut bs = bindings;
            while let Some(bs_pair) = bs.try_cast::<Pair>(cmp.mt) {
                let (e, b) = binding(cmp, &env, unsafe { bs_pair.as_ref().car });
                anf_bs.push(b);
                env = e;

                bs = unsafe { bs_pair.as_ref().cdr };
            }

            if bs == EmptyList::instance(cmp.mt).into() {
                (env, anf_bs)
            } else {
                todo!()
            }
        }

        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            let sexpr_bindings = unsafe { args.as_ref().car };

            if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                let body = unsafe { args.as_ref().car };

                if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                    let (env, bindings) = bindings(cmp, env, sexpr_bindings);

                    let bindings = bindings.iter()
                        .map(|(id, _, val_expr)| (*id, analyze_expr(cmp, &env, *val_expr)))
                        .collect();
                    let body = analyze_expr(cmp, &env, body);
                    Letrec(bindings, boxed::Box::new(body))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_if(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            let cond = analyze_expr(cmp, env, unsafe { args.as_ref().car });

            let branches = unsafe { args.as_ref().cdr };
            if let Some(branches) = branches.try_cast::<Pair>(cmp.mt) {
                let conseq = analyze_expr(cmp, env, unsafe { branches.as_ref().car });

                let branches = unsafe { branches.as_ref().cdr };
                if let Some(branches) = branches.try_cast::<Pair>(cmp.mt) {
                    let alt = analyze_expr(cmp, env, unsafe { branches.as_ref().car });

                    let branches = unsafe { branches.as_ref().cdr };
                    if branches == EmptyList::instance(cmp.mt).into() {
                        If(boxed::Box::new(cond), boxed::Box::new(conseq), boxed::Box::new(alt), anf::LiveVars::new())
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_lambda(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        // TODO: Reject duplicate parameter names:
        fn analyze_params(cmp: &mut Compiler, env: &Rc<Env>, mut params: ORef) -> (Rc<Env>, anf::Params, bool) {
            let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
            let mut env = env.clone();

            while let Some(ps_pair) = params.try_cast::<Pair>(cmp.mt) {
                if let Some(param) = unsafe { ps_pair.as_ref().car }.try_cast::<Symbol>(cmp.mt) {
                    let id = Id::fresh(cmp);
                    anf_ps.push(id);
                    env = Rc::new(Env::Binding(id, cmp.mt.root_t(param), env.clone()));
                } else {
                    todo!()
                }

                params = unsafe { ps_pair.as_ref().cdr };
            }

            if params == EmptyList::instance(cmp.mt).into() {
                (env, anf_ps, false)
            } else if let Some(param) = params.try_cast::<Symbol>(cmp.mt) {
                let id = Id::fresh(cmp);
                anf_ps.push(id);
                env = Rc::new(Env::Binding(id, cmp.mt.root_t(param), env.clone()));

                (env, anf_ps, true)
            } else {
                todo!()
            }
        }

        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            let params = unsafe { args.as_ref().car };

            if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                let body = unsafe { args.as_ref().car };

                if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                    let (env, params, varargs) = analyze_params(cmp, env, params);
                    let body = analyze_expr(cmp, &env, body);

                    r#Fn(anf::LiveVars::new(), params, varargs, boxed::Box::new(body))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_begin(cmp: &mut Compiler, env: &Rc<Env>, mut args: ORef) -> anf::Expr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            stmts.push(analyze_expr(cmp, env, unsafe { args_pair.as_ref().car }));

            args = unsafe { args_pair.as_ref().cdr };
        }

        if args == EmptyList::instance(cmp.mt).into() {
            match stmts.len() {
                0 => todo!(),
                1 => stmts.pop().unwrap(),
                _ => Begin(stmts)
            }
        } else {
            todo!()
        }
    }

    fn analyze_quote(cmp: &mut Compiler, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                Triv(Const(unsafe { cmp.mt.root(args.as_ref().car) }))
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_set(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            let dest = unsafe { args.as_ref().car };

            if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                let val_sexpr = unsafe { args.as_ref().car };

                if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                    if let Some(name) = dest.try_cast::<Symbol>(cmp.mt) {
                        let val_expr = boxed::Box::new(analyze_expr(cmp, env, val_sexpr));

                        if let Some(id) = Env::get(env, name) {
                            return Set(id, val_expr);
                        } else {
                            return GlobalSet(cmp.mt.root_t(name), val_expr);
                        }
                    }
                }
            }
        }

        todo!() // Error: argc
    }

    fn analyze_call(cmp: &mut Compiler, env: &Rc<Env>, expr: Gc<Pair>) -> anf::Expr {
        let mut bindings = Vec::new();
        let mut arg_ids = Vec::new();

        let anf_callee = analyze_expr(cmp, env, unsafe { expr.as_ref().car });
        let callee_id = Id::fresh(cmp);
        bindings.push((callee_id, anf_callee));

        let mut args = unsafe { expr.as_ref().cdr };
        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            let anf_arg = analyze_expr(cmp, env, unsafe { args_pair.as_ref().car });
            let arg_id = Id::fresh(cmp);
            bindings.push((arg_id, anf_arg));
            arg_ids.push(arg_id);

            args = unsafe { args_pair.as_ref().cdr };
        }

        if args == EmptyList::instance(cmp.mt).into() {
            Let(bindings, boxed::Box::new(Call(callee_id, arg_ids, HashSet::new())), false)
        } else {
            todo!()
        }
    }

    fn analyze_toplevel_form(cmp: &mut Compiler, form: ORef) -> anf::Expr {
        if let Some(ls) = form.try_cast::<Pair>(cmp.mt) {
            if let Some(callee) = unsafe { ls.as_ref().car }.try_cast::<Symbol>(cmp.mt) {
                if unsafe { callee.as_ref().name() == "define" } {
                    return analyze_definition(cmp, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee.as_ref().name() == "begin" } {
                    return analyze_toplevel_begin(cmp, unsafe { ls.as_ref().cdr });
                }
            }
        }

        analyze_expr(cmp, &Rc::new(Env::Empty), form)
    }

    fn analyze_definition(cmp: &mut Compiler, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
            if let Some(definiend) = unsafe { args.as_ref().car }.try_cast::<Symbol>(cmp.mt) {
                if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                    let val_sexpr = unsafe { args.as_ref().car };

                    if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                        let val_expr = analyze_expr(cmp, &Rc::new(Env::Empty), val_sexpr);

                        Define(cmp.mt.root_t(definiend), boxed::Box::new(val_expr))
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    }

    fn analyze_toplevel_begin(cmp: &mut Compiler, mut args: ORef) -> anf::Expr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            stmts.push(analyze_toplevel_form(cmp, unsafe { args_pair.as_ref().car }));

            args = unsafe { args_pair.as_ref().cdr };
        }

        if args == EmptyList::instance(cmp.mt).into() {
            match stmts.len() {
                0 => todo!(),
                1 => stmts.pop().unwrap(),
                _ => Begin(stmts)
            }
        } else {
            todo!()
        }
    }

    let body = analyze_toplevel_form(cmp, expr);

    let mut expr = r#Fn(anf::LiveVars::new(), vec![Id::fresh(cmp)], false, boxed::Box::new(body));

    let muts = mutables(&expr);

    expr = letrec(cmp, &muts, &expr);

    expr = convert_mutables(cmp, &muts, &expr); // `muts` should not have been invalidated by `letrec`

    liveness(&mut expr);

    expr    
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
            &mut Define(_, ref mut val_expr) => live_outs = live_ins(val_expr, live_outs),
            &mut GlobalSet(_, ref mut val_expr) => live_outs = live_ins(val_expr, live_outs),

            &mut Begin(ref mut stmts) =>
                for stmt in stmts.iter_mut().rev() {
                    live_outs = live_ins(stmt, live_outs);
                },

            &mut Let(ref mut bindings, ref mut body, _) => {
                live_outs = live_ins(body, live_outs);

                for (id, val) in bindings.iter_mut().rev() {
                    live_outs.remove(id);
                    live_outs = live_ins(val, live_outs);
                }
            },

            &mut If(ref mut cond, ref mut conseq, ref mut alt, ref mut if_live_outs) => {
                if_live_outs.extend(live_outs.iter());

                let conseq_outs = live_outs.clone();
                let alt_ins = live_ins(alt, live_outs);
                let mut conseq_ins = live_ins(conseq, conseq_outs);

                conseq_ins.extend(alt_ins);
                live_outs = live_ins(cond, conseq_ins);
            },

            &mut Set(..) => unreachable!(),

            &mut Box(ref mut val_expr) => live_outs = live_ins(val_expr, live_outs),

            &mut UninitializedBox => (),

            &mut BoxSet(r#box, ref mut val_expr) => {
                live_outs = live_ins(val_expr, live_outs);
                live_outs.insert(r#box);
            },

            &mut CheckedBoxSet {guard, r#box, ref mut val_expr} => {
                live_outs = live_ins(val_expr, live_outs);
                live_outs.insert(r#box);
                live_outs.insert(guard);
            },

            &mut BoxGet(r#box) => { live_outs.insert(r#box); }

            &mut CheckedBoxGet {guard, r#box} => {
                live_outs.insert(r#box);
                live_outs.insert(guard);
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

                fvs.extend(free_vars.iter());

                live_outs.extend(free_vars);
            },

            &mut Call(callee, ref args, ref mut call_live_outs) => {
                call_live_outs.extend(live_outs.iter());

                for &arg in args.iter().rev() {
                    live_outs.insert(arg);
                }

                live_outs.insert(callee);
            },

            &mut Global(_) => (),

            &mut CheckedUse {guard, id} => {
                live_outs.insert(id);
                live_outs.insert(guard);
            },

            &mut Triv(Use(id)) => { live_outs.insert(id); },

            &mut Triv(Const(_)) => (),

            &mut Letrec(..) => unreachable!()
        }

        live_outs
    }

    live_ins(expr, anf::LiveVars::new());
}
