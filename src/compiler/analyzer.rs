use std::rc::Rc;
use std::collections::hash_set::HashSet;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::compiler::anf;
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};
use crate::syntax::Syntax;

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

    fn analyze_expr(cmp: &mut Compiler, env: &Rc<Env>, sexpr: ORef) -> anf::Expr {
        let stx = sexpr.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        let expr = unsafe { stx.as_ref().expr };

        if let Some(name) = expr.try_cast::<Symbol>(cmp.mt) {
            if let Some(id) = Env::get(env, name) {
                Triv(Use(id))
            } else {
                Global(cmp.mt.root_t(name))
            }
        } else if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
            let callee = unsafe { ls.as_ref().car }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee_sym) = unsafe { callee.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                if unsafe { callee_sym.as_ref().name() == "if" } {
                    return analyze_if(cmp, env, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee_sym.as_ref().name() == "lambda" } {
                    return analyze_lambda(cmp, env, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee_sym.as_ref().name() == "begin" } {
                    return analyze_begin(cmp, env, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee_sym.as_ref().name() == "letrec" } {
                    return analyze_letrec(cmp, env, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee_sym.as_ref().name() == "quote" } {
                    return analyze_quote(cmp, unsafe { ls.as_ref().cdr });
                } else if unsafe { callee_sym.as_ref().name() == "set!" } {
                    return analyze_set(cmp, env, unsafe { ls.as_ref().cdr });
                }
            }
                
            analyze_call(cmp, env, ls)
        } else if expr == EmptyList::instance(cmp.mt).into() {
            todo!() // error
        } else {
            let c = expr.to_datum(cmp.mt);
            Triv(Const(cmp.mt.root(c)))
        }
    }

    fn analyze_letrec(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
        fn bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef) -> (Rc<Env>, Vec<(Id, Gc<Symbol>, ORef)>) {
            fn binding(cmp: &mut Compiler, env: &Rc<Env>, binding: ORef) -> (Rc<Env>, (Id, Gc<Symbol>, ORef)) {
                let stx = binding.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });

                if let Some(binding) = unsafe { stx.as_ref().expr }.try_cast::<Pair>(cmp.mt) {
                    let pat = unsafe { binding.as_ref().car };

                    if let Some(binding) = unsafe { binding.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                        let val = unsafe { binding.as_ref().car };

                        if unsafe { binding.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                            let identifier = pat.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                                todo!() // error
                            });

                            if let Some(name) = unsafe { identifier.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
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

            let mut bs = match bindings.try_cast::<Syntax>(cmp.mt) {
                Some(bs) => unsafe { bs.as_ref().expr },
                None => todo!() // error
            };
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
        fn analyze_params(cmp: &mut Compiler, env: &Rc<Env>, params: ORef) -> (Rc<Env>, anf::Params, bool) {
            let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
            let mut env = env.clone();

            let params = params.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(param) = unsafe { params.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                let id = {
                    let param = cmp.mt.root_t(param);
                    Id::src_fresh(cmp, param)
                };
                anf_ps.push(id);
                env = Rc::new(Env::Binding(id, cmp.mt.root_t(param), env.clone()));

                return (env, anf_ps, true)
            } else {
                let mut params = unsafe { params.as_ref().expr };

                while let Some(ps_pair) = params.try_cast::<Pair>(cmp.mt) {
                    let param = unsafe { ps_pair.as_ref().car }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                        todo!() // error
                    });

                    if let Some(param) = unsafe { param.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
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
                    return (env, anf_ps, false)
                } else if let Some(param) = params.try_cast::<Syntax>(cmp.mt) {
                    if let Some(param) = unsafe { param.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                        let id = {
                            let param = cmp.mt.root_t(param);
                            Id::src_fresh(cmp, param)
                        };
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, cmp.mt.root_t(param), env.clone()));

                        return (env, anf_ps, true)
                    }
                }
            }

            todo!() // error
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
                let c = unsafe { cmp.mt.root(args.as_ref().car) }.to_datum(cmp.mt);
                Triv(Const(cmp.mt.root(c)))
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
                    let dest = dest.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                        todo!() // error
                    });

                    if let Some(name) = unsafe { dest.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                        let val_expr = boxed::Box::new(analyze_expr(cmp, env, val_sexpr));

                        if let Some(id) = Env::get(env, name) {
                            return Set(id, val_expr);
                        } else {
                            return GlobalSet(cmp.mt.root_t(name), val_expr);
                        }
                    } else {
                        todo!() // error: dest not an identifier
                    }
                }
            }
        }

        todo!() // Error: argc
    }

    fn analyze_call(cmp: &mut Compiler, env: &Rc<Env>, expr: Gc<Pair>) -> anf::Expr {
        let mut bindings = Vec::new();

        let anf_callee = analyze_expr(cmp, env, unsafe { expr.as_ref().car });
        bindings.push((Id::fresh(cmp), anf_callee));

        let mut args = unsafe { expr.as_ref().cdr };
        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            let anf_arg = analyze_expr(cmp, env, unsafe { args_pair.as_ref().car });
            bindings.push((Id::fresh(cmp), anf_arg));

            args = unsafe { args_pair.as_ref().cdr };
        }

        if args == EmptyList::instance(cmp.mt).into() {
            Call(bindings, HashSet::new())
        } else {
            todo!()
        }
    }

    fn analyze_toplevel_form(cmp: &mut Compiler, form: ORef) -> anf::Expr {
        let stx = form.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        if let Some(ls) = unsafe { stx.as_ref().expr }.try_cast::<Pair>(cmp.mt) {
            let callee = unsafe { ls.as_ref().car }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee) = unsafe { callee.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
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
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = unsafe { args.as_ref().car };
        let args = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let val_expr = unsafe { args.as_ref().car };
        if unsafe { args.as_ref().cdr } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let definiend = match definiend.try_cast::<Syntax>(cmp.mt) {
            Some(definiend) => unsafe { definiend.as_ref().expr }.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            }),
            None => todo!() // error
        };
        let val_expr = analyze_expr(cmp, &Rc::new(Env::Empty), val_expr);

        Define(cmp.mt.root_t(definiend), boxed::Box::new(val_expr))
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
