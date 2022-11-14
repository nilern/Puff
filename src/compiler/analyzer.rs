use std::rc::Rc;
use std::collections::hash_set::HashSet;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::compiler::anf::{self, PosExpr};
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::{Handle, HandleAny, Root, root};
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};
use crate::syntax::Syntax;

pub fn analyze(cmp: &mut Compiler, expr: HandleAny) -> anf::PosExpr {
    use anf::Expr::*;
    use anf::Triv::*;

    enum Env {
        Binding(Id, Handle<Symbol>, Rc<Env>),
        Empty
    }

    impl Env {
        fn get(env: &Rc<Env>, name: Gc<Symbol>) -> Option<Id> {
            let mut env = env.clone();
            loop {
                match &*env {
                    &Self::Binding(id, ref bname, ref parent) =>
                        if bname.oref() == name {
                            return Some(id);
                        } else {
                            env = parent.clone();
                        },
                    &Self::Empty => return None
                }
            }
        }
    }

    fn analyze_expr(cmp: &mut Compiler, env: &Rc<Env>, sexpr: HandleAny) -> anf::PosExpr {
        let stx = sexpr.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        let expr = stx.expr;
        let pos = root!(&mut cmp.mt, stx.pos);

        if let Some(name) = expr.try_cast::<Symbol>(cmp.mt) {
            if let Some(id) = Env::get(env, name) {
                PosExpr {expr: Triv(Use(id)), pos}
            } else {
                PosExpr {expr: Global(root!(&mut cmp.mt, name)), pos}
            }
        } else if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
            let callee = cmp.mt.borrow(ls).car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee_sym) = cmp.mt.borrow(callee).expr.try_cast::<Symbol>(cmp.mt) {
                if cmp.mt.borrow(callee_sym).name() == "if" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_if(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "lambda" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_lambda(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "case-lambda" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_case_lambda(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "begin" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_begin(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "letrec" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_letrec(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "quote" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_quote(cmp, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "set!" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_set(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee_sym).name() == "call-with-values*" {
                    // TODO: Manually assemble bytecode for `call-with-values` instead.
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
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

    fn analyze_letrec(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        fn bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef) -> (Rc<Env>, Vec<(Id, HandleAny)>) {
            fn binding(cmp: &mut Compiler, env: &Rc<Env>, binding: ORef) -> (Rc<Env>, (Id, HandleAny)) {
                let stx = binding.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let binding = cmp.mt.borrow(stx).expr.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let pat = cmp.mt.borrow(binding).car();
                let binding = cmp.mt.borrow(binding).cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let val = root!(cmp.mt, cmp.mt.borrow(binding).car());
                if cmp.mt.borrow(binding).cdr() != EmptyList::instance(cmp.mt).into() {
                    todo!() // error
                }

                let identifier = pat.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                    todo!() // error
                });
                let name = root!(cmp.mt, cmp.mt.borrow(identifier).expr.try_cast::<Symbol>(cmp.mt)
                    .unwrap_or_else(|| {
                        todo!() // error
                    }));
                let id = Id::src_fresh(cmp, name.clone());

                (Rc::new(Env::Binding(id, name, env.clone())), (id, val))
            }

            let mut anf_bs = Vec::new();
            let mut env = env.clone();

            let bs = bindings.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });
            let mut bs = cmp.mt.borrow(bs).expr;
            while let Some(bs_pair) = bs.try_cast::<Pair>(cmp.mt) {
                let (e, b) = binding(cmp, &env, cmp.mt.borrow(bs_pair).car());
                anf_bs.push(b);
                env = e;

                bs = cmp.mt.borrow(bs_pair).cdr();
            }

            if bs != EmptyList::instance(cmp.mt).into() {
                todo!()
            }

            (env, anf_bs)
        }

        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let sexpr_bindings = args.car();
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let body = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
            todo!("error: body length > 1");
        }

        let (env, bindings) = bindings(cmp, env, sexpr_bindings);
        let bindings = bindings.into_iter()
            .map(|(id, val_expr)| (id, analyze_expr(cmp, &env, val_expr)))
            .collect();
        let body = analyze_expr(cmp, &env, body);
        PosExpr {expr: Letrec(bindings, boxed::Box::new(body)), pos}
    }

    fn analyze_if(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let cond = root!(cmp.mt, args.car());
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let conseq = root!(cmp.mt, cmp.mt.borrow(args).car());
        let args = cmp.mt.borrow(args).cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let alt = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
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

    fn analyze_lambda_clause(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny)
        -> (HandleAny, HashSet<Id>, Vec<Id>, bool, PosExpr)
    {
        // TODO: Reject duplicate parameter names:
        fn analyze_params(cmp: &mut Compiler, env: &Rc<Env>, params: ORef) -> (HandleAny, Rc<Env>, anf::Params, bool) {
            let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
            let mut env = env.clone();

            let params = params.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });
            let pos = root!(cmp.mt, cmp.mt.borrow(params).pos);

            if let Some(param) = cmp.mt.borrow(params).expr.try_cast::<Symbol>(cmp.mt) { // (lambda args ...
                let param = root!(cmp.mt, param);
                let id = Id::src_fresh(cmp, param.clone());
                anf_ps.push(id);

                return (pos, Rc::new(Env::Binding(id, param, env.clone())), anf_ps, true)
            } else { // (lambda (...
                let mut params = cmp.mt.borrow(params).expr;

                while let Some(ps_pair) = params.try_cast::<Pair>(cmp.mt) {
                    let param = cmp.mt.borrow(ps_pair).car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                        todo!() // error
                    });

                    if let Some(param) = cmp.mt.borrow(param).expr.try_cast::<Symbol>(cmp.mt) {
                        let param = root!(cmp.mt, param);
                        let id = Id::src_fresh(cmp, param.clone());
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, param, env.clone()));
                    } else {
                        todo!()
                    }

                    params = cmp.mt.borrow(ps_pair).cdr();
                }

                if params == EmptyList::instance(cmp.mt).into() { // (lambda (arg*) ...
                    return (pos, env, anf_ps, false)
                } else if let Some(param) = params.try_cast::<Syntax>(cmp.mt) { // (lambda (arg* . args) ...
                    if let Some(param) = cmp.mt.borrow(param).expr.try_cast::<Symbol>(cmp.mt) {
                        let param = root!(cmp.mt, param);
                        let id = Id::src_fresh(cmp, param.clone());
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, param, env.clone()));

                        return (pos, env, anf_ps, true)
                    }
                }
            }

            todo!() // error
        }

        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let params = args.car();
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let body = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let (pos, env, params, varargs) = analyze_params(cmp, env, params);
        let body = analyze_expr(cmp, &env, body);

        (pos, anf::LiveVars::new(), params, varargs, body)
    }

    fn analyze_lambda(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let (_, free_vars, params, varargs, body) = analyze_lambda_clause(cmp, env, args);
        PosExpr {expr: r#Fn(free_vars, params, varargs, boxed::Box::new(body)), pos}
    }

    fn analyze_case_lambda(cmp: &mut Compiler, env: &Rc<Env>, mut args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let mut clauses = Vec::new();

        while let Some(args_pair) = args.clone().try_cast::<Pair>(cmp.mt) {
            let clause = root!(cmp.mt, cmp.mt.borrow(args_pair.car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!()
            })).expr);
            clauses.push(analyze_lambda_clause(cmp, env, clause));

            args = root!(cmp.mt, args_pair.cdr());
        }

        if args.oref() == EmptyList::instance(cmp.mt).into() {
            match clauses.len() {
                1 => {
                    let (_, free_vars, params, varargs, body) = clauses.pop().unwrap();
                    PosExpr {expr: r#Fn(free_vars, params, varargs, boxed::Box::new(body)), pos}
                },
                _ => PosExpr {expr: CaseFn(clauses), pos}
            }
        } else {
            todo!("error: case-lambda clauses")
        }
    }

    fn analyze_begin(cmp: &mut Compiler, env: &Rc<Env>, mut args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.clone().try_cast::<Pair>(cmp.mt) {
            let stmt = root!(cmp.mt, args_pair.car());
            stmts.push(analyze_expr(cmp, env, stmt));

            args = root!(cmp.mt, args_pair.cdr());
        }

        if args.oref() == EmptyList::instance(cmp.mt).into() {
            match stmts.len() {
                0 => todo!(),
                1 => stmts.pop().unwrap(),
                _ => PosExpr {expr: Begin(stmts), pos}
            }
        } else {
            todo!()
        }
    }

    fn analyze_quote(cmp: &mut Compiler, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        if args.cdr() != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let c = root!(cmp.mt, args.car().to_datum(cmp.mt));

        PosExpr {expr: Triv(Const(c)), pos}
    }

    fn analyze_set(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let dest = args.car();
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let val_sexpr = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let dest = dest.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let name = root!(cmp.mt, cmp.mt.borrow(dest).expr.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        }));
        let val_expr = boxed::Box::new(analyze_expr(cmp, env, val_sexpr));

        if let Some(id) = Env::get(env, name.oref()) {
            return PosExpr {expr: Set(id, val_expr), pos};
        } else {
            return PosExpr {expr: GlobalSet(name, val_expr), pos};
        }
    }

    fn analyze_call_with_values(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let producer = root!(cmp.mt, args.car());
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let consumer = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
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

    fn analyze_call(cmp: &mut Compiler, env: &Rc<Env>, expr: Handle<Pair>, pos: HandleAny) -> anf::PosExpr {
        let mut bindings = Vec::new();

        let callee = root!(cmp.mt, expr.car());
        let anf_callee = analyze_expr(cmp, env, callee);
        bindings.push((Id::fresh(cmp), anf_callee));

        let mut args = root!(cmp.mt, expr.cdr());
        while let Some(args_pair) = args.clone().try_cast::<Pair>(cmp.mt) {
            let arg = root!(cmp.mt, args_pair.car());
            let anf_arg = analyze_expr(cmp, env, arg);
            bindings.push((Id::fresh(cmp), anf_arg));

            args = root!(cmp.mt, args_pair.cdr());
        }

        if args.oref() != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        PosExpr {expr: Call(bindings, HashSet::new()), pos}
    }

    fn analyze_toplevel_form(cmp: &mut Compiler, form: HandleAny) -> anf::PosExpr {
        let stx = form.clone().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let pos = root!(&mut cmp.mt, stx.pos);

        if let Some(ls) = stx.expr.try_cast::<Pair>(cmp.mt) {
            let callee = cmp.mt.borrow(ls).car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee) = cmp.mt.borrow(callee).expr.try_cast::<Symbol>(cmp.mt) {
                if cmp.mt.borrow(callee).name() == "define" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_definition(cmp, args, pos);
                } else if cmp.mt.borrow(callee).name() == "begin" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_toplevel_begin(cmp, args, pos);
                }
            }
        }

        analyze_expr(cmp, &Rc::new(Env::Empty), form)
    }

    fn analyze_definition(cmp: &mut Compiler, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = args.car();
        let args = args.cdr().try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let val_expr = root!(cmp.mt, cmp.mt.borrow(args).car());
        if cmp.mt.borrow(args).cdr() != EmptyList::instance(cmp.mt).into() {
            todo!() // error
        }

        let definiend = definiend.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = root!(cmp.mt, cmp.mt.borrow(definiend).expr.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        }));
        let val_expr = analyze_expr(cmp, &Rc::new(Env::Empty), val_expr);

        PosExpr {expr: Define(definiend, boxed::Box::new(val_expr)), pos}
    }

    fn analyze_toplevel_begin(cmp: &mut Compiler, mut args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.clone().try_cast::<Pair>(cmp.mt) {
            let stmt = root!(cmp.mt, args_pair.car());
            stmts.push(analyze_toplevel_form(cmp, stmt));

            args = root!(cmp.mt, args_pair.cdr());
        }

        if args.oref() != EmptyList::instance(cmp.mt).into() {
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
