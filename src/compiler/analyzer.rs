use std::rc::Rc;
use std::collections::hash_set::HashSet;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::compiler::anf::{self, PosExpr};
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::{HandleT, Handle, Root, root};
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};
use crate::syntax::Syntax;

pub fn analyze(cmp: &mut Compiler, expr: Handle) -> anf::PosExpr {
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

    fn analyze_expr(cmp: &mut Compiler, env: &Rc<Env>, sexpr: Handle) -> anf::PosExpr {
        let stx = sexpr.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        let expr = unsafe { stx.as_ref().expr };
        let pos = root!(&mut cmp.mt, unsafe { stx.as_ref().pos });

        if let Some(name) = expr.try_cast::<Symbol>(cmp.mt) {
            if let Some(id) = Env::get(env, name) {
                PosExpr {expr: Triv(Use(id)), pos}
            } else {
                PosExpr {expr: Global(root!(&mut cmp.mt, name)), pos}
            }
        } else if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
            let callee = unsafe { ls.as_ref().car() }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee_sym) = unsafe { callee.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                if unsafe { callee_sym.as_ref().name() == "if" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_if(cmp, env, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "lambda" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_lambda(cmp, env, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "begin" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_begin(cmp, env, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "letrec" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_letrec(cmp, env, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "quote" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_quote(cmp, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "set!" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_set(cmp, env, args, pos);
                } else if unsafe { callee_sym.as_ref().name() == "call-with-values*" } {
                    // TODO: Manually assemble bytecode for `call-with-values` instead.
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_call_with_values(cmp, env, args, pos);
                }
            }
            
            let ls = root!(cmp.mt, ls);
            analyze_call(cmp, env, ls, pos)
        } else if expr == EmptyList::instance(cmp.mt).into() {
            todo!() // error
        } else {
            let c = expr.to_datum(cmp.mt);
            PosExpr {expr: Triv(Const(root!(&mut cmp.mt, c))), pos}
        }
    }

    fn analyze_letrec(cmp: &mut Compiler, env: &Rc<Env>, args: Handle, pos: Handle) -> anf::PosExpr {
        fn bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef)
            -> (Rc<Env>, Vec<(Id, HandleT<Symbol>, Handle)>)
        {
            fn binding(cmp: &mut Compiler, env: &Rc<Env>, binding: ORef) -> (Rc<Env>, (Id, HandleT<Symbol>, Handle)) {
                let stx = binding.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let binding = unsafe { stx.as_ref().expr }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let pat = unsafe { binding.as_ref().car() };
                let binding = unsafe { binding.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let val = root!(cmp.mt, unsafe { binding.as_ref().car() });
                if unsafe { binding.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
                    todo!() // error
                }

                let identifier = pat.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let name = root!(cmp.mt, unsafe { identifier.as_ref().expr }.try_cast::<Symbol>(cmp.mt)
                    .unwrap_or_else(|| {
                        todo!() // error
                    }));
                let id = Id::src_fresh(cmp, name.clone());

                (Rc::new(Env::Binding(id, name.clone(), env.clone())), (id, name, val))
            }

            let mut anf_bs = Vec::new();
            let mut env = env.clone();

            let bs = bindings.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });
            let mut bs = unsafe { bs.as_ref().expr };
            while let Some(bs_pair) = bs.try_cast::<Pair>(cmp.mt) {
                let (e, b) = binding(cmp, &env, unsafe { bs_pair.as_ref().car() });
                anf_bs.push(b);
                env = e;

                bs = unsafe { bs_pair.as_ref().cdr() };
            }

            if bs != EmptyList::instance(cmp.mt).into() {
                todo!()
            }

            (env, anf_bs)
        }

        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let sexpr_bindings = unsafe { args.as_ref().car() };
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let body = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let (env, bindings) = bindings(cmp, env, sexpr_bindings);
        let bindings = bindings.into_iter()
            .map(|(id, _, val_expr)| (id, analyze_expr(cmp, &env, val_expr)))
            .collect();
        let body = analyze_expr(cmp, &env, body);
        PosExpr {expr: Letrec(bindings, boxed::Box::new(body)), pos}
    }

    fn analyze_if(cmp: &mut Compiler, env: &Rc<Env>, args: Handle, pos: Handle) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let cond = root!(cmp.mt, unsafe { args.as_ref().car() });
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let conseq = root!(cmp.mt, unsafe { args.as_ref().car() });
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let alt = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let cond = analyze_expr(cmp, env, cond);
        let conseq = analyze_expr(cmp, env, conseq);
        let alt = analyze_expr(cmp, env, alt);

        PosExpr {
            expr: If(boxed::Box::new(cond), boxed::Box::new(conseq), boxed::Box::new(alt), anf::LiveVars::new()),
            pos
        }
    }

    fn analyze_lambda(cmp: &mut Compiler, env: &Rc<Env>, args: Handle, pos: Handle) -> anf::PosExpr {
        // TODO: Reject duplicate parameter names:
        fn analyze_params(cmp: &mut Compiler, env: &Rc<Env>, params: ORef) -> (Rc<Env>, anf::Params, bool) {
            let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
            let mut env = env.clone();

            let params = params.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(param) = unsafe { params.as_ref().expr }.try_cast::<Symbol>(cmp.mt) { // (lambda args ...
                let param = root!(cmp.mt, param);
                let id = Id::src_fresh(cmp, param.clone());
                anf_ps.push(id);

                return (Rc::new(Env::Binding(id, param, env.clone())), anf_ps, true)
            } else { // (lambda (...
                let mut params = unsafe { params.as_ref().expr };

                while let Some(ps_pair) = params.try_cast::<Pair>(cmp.mt) {
                    let param = unsafe { ps_pair.as_ref().car() }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                        todo!() // error
                    });

                    if let Some(param) = unsafe { param.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                        let param = root!(cmp.mt, param);
                        let id = Id::src_fresh(cmp, param.clone());
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, param, env.clone()));
                    } else {
                        todo!()
                    }

                    params = unsafe { ps_pair.as_ref().cdr() };
                }

                if params == EmptyList::instance(cmp.mt).into() { // (lambda (arg*) ...
                    return (env, anf_ps, false)
                } else if let Some(param) = params.try_cast::<Syntax>(cmp.mt) { // (lambda (arg* . args) ...
                    if let Some(param) = unsafe { param.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                        let param = root!(cmp.mt, param);
                        let id = Id::src_fresh(cmp, param.clone());
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, param, env.clone()));

                        return (env, anf_ps, true)
                    }
                }
            }

            todo!() // error
        }

        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let params = unsafe { args.as_ref().car() };
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let body = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let (env, params, varargs) = analyze_params(cmp, env, params);
        let body = analyze_expr(cmp, &env, body);

        PosExpr {expr: r#Fn(anf::LiveVars::new(), params, varargs, boxed::Box::new(body)), pos}
    }

    fn analyze_begin(cmp: &mut Compiler, env: &Rc<Env>, mut args: Handle, pos: Handle) -> anf::PosExpr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            let stmt = root!(cmp.mt, unsafe { args_pair.as_ref().car() });
            stmts.push(analyze_expr(cmp, env, stmt));

            args = root!(cmp.mt, unsafe { args_pair.as_ref().cdr() });
        }

        if *args == EmptyList::instance(cmp.mt).into() {
            match stmts.len() {
                0 => todo!(),
                1 => stmts.pop().unwrap(),
                _ => PosExpr {expr: Begin(stmts), pos}
            }
        } else {
            todo!()
        }
    }

    fn analyze_quote(cmp: &mut Compiler, args: Handle, pos: Handle) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let c = root!(cmp.mt, unsafe { args.as_ref().car() }.to_datum(cmp.mt));

        PosExpr {expr: Triv(Const(c)), pos}
    }

    fn analyze_set(cmp: &mut Compiler, env: &Rc<Env>, args: Handle, pos: Handle) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let dest = unsafe { args.as_ref().car() };
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let val_sexpr = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let dest = dest.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let name = root!(cmp.mt, unsafe { dest.as_ref().expr }.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        }));
        let val_expr = boxed::Box::new(analyze_expr(cmp, env, val_sexpr));

        if let Some(id) = Env::get(env, *name) {
            return PosExpr {expr: Set(id, val_expr), pos};
        } else {
            return PosExpr {expr: GlobalSet(root!(&mut cmp.mt, name), val_expr), pos};
        }
    }

    fn analyze_call_with_values(cmp: &mut Compiler, env: &Rc<Env>, args: Handle, pos: Handle) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let producer = root!(cmp.mt, unsafe { args.as_ref().car() });
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let consumer = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let producer = analyze_expr(cmp, env, producer);
        let pid = Id::fresh(cmp);
        let consumer = analyze_expr(cmp, env, consumer);
        let cid = Id::fresh(cmp);
        PosExpr {
            expr: CallWithValues((pid, boxed::Box::new(producer)), (cid, boxed::Box::new(consumer)),
                anf::LiveVars::new()),
            pos
        }
    }

    fn analyze_call(cmp: &mut Compiler, env: &Rc<Env>, expr: HandleT<Pair>, pos: Handle) -> anf::PosExpr {
        let mut bindings = Vec::new();

        let callee = root!(cmp.mt, unsafe { expr.as_ref().car() });
        let anf_callee = analyze_expr(cmp, env, callee);
        bindings.push((Id::fresh(cmp), anf_callee));

        let mut args = root!(cmp.mt, unsafe { expr.as_ref().cdr() });
        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            let arg = root!(cmp.mt, unsafe { args_pair.as_ref().car() });
            let anf_arg = analyze_expr(cmp, env, arg);
            bindings.push((Id::fresh(cmp), anf_arg));

            args = root!(cmp.mt, unsafe { args_pair.as_ref().cdr() });
        }

        if *args != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        PosExpr {expr: Call(bindings, HashSet::new()), pos}
    }

    fn analyze_toplevel_form(cmp: &mut Compiler, form: Handle) -> anf::PosExpr {
        let stx = form.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let expr = unsafe { stx.as_ref().expr };
        let pos = root!(&mut cmp.mt, unsafe { stx.as_ref().pos });

        if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
            let callee = unsafe { ls.as_ref().car() }.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee) = unsafe { callee.as_ref().expr }.try_cast::<Symbol>(cmp.mt) {
                if unsafe { callee.as_ref().name() == "define" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_definition(cmp, args, pos);
                } else if unsafe { callee.as_ref().name() == "begin" } {
                    let args = root!(cmp.mt, unsafe { ls.as_ref().cdr() });
                    return analyze_toplevel_begin(cmp, args, pos);
                }
            }
        }

        analyze_expr(cmp, &Rc::new(Env::Empty), form)
    }

    fn analyze_definition(cmp: &mut Compiler, args: Handle, pos: Handle) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = root!(cmp.mt, unsafe { args.as_ref().car() });
        let args = unsafe { args.as_ref().cdr() }.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let val_expr = root!(cmp.mt, unsafe { args.as_ref().car() });
        if unsafe { args.as_ref().cdr() } != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let definiend = definiend.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = root!(cmp.mt, unsafe { definiend.as_ref().expr }.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        }));
        let val_expr = analyze_expr(cmp, &Rc::new(Env::Empty), val_expr);

        PosExpr {expr: Define(definiend, boxed::Box::new(val_expr)), pos}
    }

    fn analyze_toplevel_begin(cmp: &mut Compiler, mut args: Handle, pos: Handle) -> anf::PosExpr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            let stmt = root!(cmp.mt, unsafe { args_pair.as_ref().car() });
            stmts.push(analyze_toplevel_form(cmp, stmt));

            args = root!(cmp.mt, unsafe { args_pair.as_ref().cdr() });
        }

        if *args != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        match stmts.len() {
            0 => todo!(),
            1 => stmts.pop().unwrap(),
            _ => PosExpr {expr: Begin(stmts), pos}
        }
    }

    let body = analyze_toplevel_form(cmp, expr);
    let body_pos = body.pos.clone();
    PosExpr {expr: r#Fn(anf::LiveVars::new(), vec![Id::fresh(cmp)], false, boxed::Box::new(body)), pos: body_pos}
}
