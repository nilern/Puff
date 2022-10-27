use std::rc::Rc;
use std::collections::hash_set::HashSet;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::anf;
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};

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
                                let id = {
                                    let name = cmp.mt.root_t(name);
                                    Id::src_fresh(cmp, name)
                                };
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
                                let id = {
                                    let name = cmp.mt.root_t(name);
                                    Id::src_fresh(cmp, name)
                                };
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
                    let id = {
                        let param = cmp.mt.root_t(param);
                        Id::src_fresh(cmp, param)
                    };
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
                let id = {
                    let param = cmp.mt.root_t(param);
                    Id::src_fresh(cmp, param)
                };
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

    r#Fn(anf::LiveVars::new(), vec![Id::fresh(cmp)], false, boxed::Box::new(body))
}
