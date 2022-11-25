use std::rc::Rc;
use std::collections::hash_set::HashSet;
use std::collections::hash_map::HashMap;
use std::boxed;

use crate::list::{Pair, EmptyList};
use crate::compiler::anf::{self, PosExpr, Domain};
use crate::heap_obj::Singleton;
use crate::oref::{ORef, Gc};
use crate::handle::{Handle, HandleAny, Root, root};
use crate::symbol::Symbol;
use crate::compiler::{Compiler, Id};
use crate::syntax::Syntax;
use crate::mutator::Mutator;

pub fn analyze(cmp: &mut Compiler, expr: HandleAny) -> anf::PosExpr {
    use anf::Expr::*;
    use anf::Triv::*;

    type UnitToplevel = HashMap<String, Handle<Symbol>>; // OPTIMIZE: hash set of `Handle<Symbol>` somehow

    enum Binding {
        Local(Id),
        UnitGlobal,
        OldGlobal
    }

    enum Env {
        Binding(Id, Handle<Symbol>, Rc<Env>),
        UnitToplevel(UnitToplevel)
    }

    fn binding(mt: &mut Mutator, env: &Rc<Env>, name: Gc<Symbol>) -> Option<Binding> {
        let mut env = env.clone();
        loop {
            match &*env {
                &Env::Binding(id, ref bname, ref parent) =>
                    if bname.oref() == name {
                        return Some(Binding::Local(id));
                    } else {
                        env = parent.clone();
                    },

                &Env::UnitToplevel(ref unit_toplevel) =>
                    return if unit_toplevel.contains_key(mt.borrow(name).name()) {
                        Some(Binding::UnitGlobal)
                    } else if mt.ns().contains_key(name) {
                        Some(Binding::OldGlobal)
                    } else {
                        None
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
            match binding(&mut cmp.mt, env, name) {
                Some(Binding::Local(id)) => PosExpr {expr: Triv(Use(id)), pos},
                Some(Binding::UnitGlobal) => PosExpr {expr: Global(root!(&mut cmp.mt, name)), pos},

                // OPTIMIZE: Could emit direct Var usage instead of Namespace-lookup instructions:
                Some(Binding::OldGlobal) => PosExpr {expr: Global(root!(&mut cmp.mt, name)), pos},

                None => {
                    eprintln!("Warning: unbound variable `{}` at {}", cmp.mt.borrow(name).name(),
                        pos.oref().within(cmp.mt));
                    PosExpr {expr: Global(root!(&mut cmp.mt, name)), pos}
                }
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
        // TODO: Reject duplicate binding names:
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
        -> (HandleAny, Option<Domain>, HashSet<Id>, Vec<Id>, bool, PosExpr)
    {
        fn analyze_typed_param(cmp: &mut Compiler, outer_env: &Rc<Env>, args: ORef) -> (Handle<Symbol>, PosExpr) {
            let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| todo!("error"));

            let param = cmp.mt.borrow(args).car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!("error: param not a syntax object")
            });
            let param = root!(cmp.mt, cmp.mt.borrow(param).expr.try_cast::<Symbol>(cmp.mt)
                .unwrap_or_else(|| todo!("error: param.expr not a symbol")));

            let args = root!(cmp.mt, cmp.mt.borrow(args).cdr().try_cast::<Pair>(cmp.mt)
                .unwrap_or_else(|| todo!()));

            let param_type = root!(cmp.mt, args.car());
            let param_type = analyze_expr(cmp, outer_env, param_type);

            if args.cdr() != EmptyList::instance(cmp.mt).into() {
                todo!("error")
            }

            (param, param_type)
        }

        // TODO: Reject duplicate parameter names:
        fn analyze_params(cmp: &mut Compiler, outer_env: &Rc<Env>, params: ORef)
            -> (HandleAny, Rc<Env>, Option<Domain>, anf::Params, bool)
        {
            let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
            let mut env = outer_env.clone();
            let mut varargs = false;
            let mut domain = Vec::new();

            let params = params.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!("error: params not a syntax object")
            });
            let pos = root!(cmp.mt, cmp.mt.borrow(params).pos);
            let params = cmp.mt.borrow(params).expr;

            if let Some(param) = params.try_cast::<Symbol>(cmp.mt) { // (lambda args ...
                let param = root!(cmp.mt, param);
                let id = Id::src_fresh(cmp, param.clone());
                anf_ps.push(id);
                env = Rc::new(Env::Binding(id, param, env.clone()));
                varargs = true;
                domain.push(None);
            } else { // (lambda (...
                let mut params = root!(cmp.mt, params);

                while let Some(ps_pair) = params.clone().try_cast::<Pair>(cmp.mt) {
                    let car = ps_pair.car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                        todo!("error: param not a syntax object")
                    });
                    if let Some(car) = cmp.mt.borrow(car).expr.try_cast::<Symbol>(cmp.mt) {
                        if cmp.mt.borrow(car).name() != ":" {
                            let param = root!(cmp.mt, car);
                            let id = Id::src_fresh(cmp, param.clone());
                            anf_ps.push(id);
                            env = Rc::new(Env::Binding(id, param, env.clone()));
                            domain.push(None);
                        } else {
                            let (param, param_type) = analyze_typed_param(cmp, outer_env, ps_pair.cdr());

                            let id = Id::src_fresh(cmp, param.clone());
                            anf_ps.push(id);
                            env = Rc::new(Env::Binding(id, param, env.clone()));
                            varargs = true;
                            domain.push(Some(param_type));

                            let domain = if domain.iter().any(Option::is_some) { Some(domain) } else { None };
                            return (pos, env, domain, anf_ps, varargs)
                        }
                    } else if let Some(param_pair) = cmp.mt.borrow(car).expr.try_cast::<Pair>(cmp.mt) {
                        let (param, param_type) = analyze_typed_param (cmp, outer_env, cmp.mt.borrow(param_pair).cdr());

                        let id = Id::src_fresh(cmp, param.clone());
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, param, env.clone()));
                        domain.push(Some(param_type));
                    } else {
                        todo!("error: param not a symbol or pair");
                    }

                    params = root!(cmp.mt, ps_pair.cdr());
                }

                if params.oref() == EmptyList::instance(cmp.mt).into() { // (lambda (arg*) ...
                    // Done
                } else if let Some(param) = params.try_cast::<Syntax>(cmp.mt) { // (lambda (arg* . args) ...
                    let param = root!(cmp.mt, param.expr.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
                        todo!("error params.expr tail not a list or a symbol")
                    }));

                    let id = Id::src_fresh(cmp, param.clone());
                    anf_ps.push(id);
                    env = Rc::new(Env::Binding(id, param, env.clone()));
                    varargs = true;
                    domain.push(None);
                } else {
                    todo!("error: params.expr tail not a list or symbol");
                }
            }

            let domain = if domain.iter().any(Option::is_some) { Some(domain) } else { None };
            (pos, env, domain, anf_ps, varargs)
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

        let (pos, env, domain, params, varargs) = analyze_params(cmp, env, params);
        let body = analyze_expr(cmp, &env, body);

        (pos, domain, anf::LiveVars::new(), params, varargs, body)
    }

    fn analyze_lambda(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let (_, domain, free_vars, params, varargs, body) = analyze_lambda_clause(cmp, env, args);
        PosExpr {expr: r#Fn(domain, free_vars, params, varargs, boxed::Box::new(body)), pos}
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
                    let (_, domain, free_vars, params, varargs, body) = clauses.pop().unwrap();
                    PosExpr {expr: r#Fn(domain, free_vars, params, varargs, boxed::Box::new(body)), pos}
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

        match binding(&mut cmp.mt, env, name.oref()) {
            Some(Binding::Local(id)) => PosExpr {expr: Set(id, val_expr), pos},
            Some(Binding::UnitGlobal) => PosExpr {expr: GlobalSet(name, val_expr), pos},

            // OPTIMIZE: Could emit direct Var usage instead of Namespace-lookup instructions:
            Some(Binding::OldGlobal) => PosExpr {expr: GlobalSet(name, val_expr), pos},

            None => {
                eprintln!("Warning: `set!` unbound variable `{}` at {}", name.name(), pos.oref().within(cmp.mt));
                PosExpr {expr: GlobalSet(name, val_expr), pos}
            }
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

    fn declare_toplevel_form(cmp: &mut Compiler, unit_toplevel: &mut UnitToplevel, form: ORef) {
        let stx = form.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        if let Some(ls) = cmp.mt.borrow(stx).expr.try_cast::<Pair>(cmp.mt) {
            let callee = cmp.mt.borrow(ls).car().try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
                todo!() // error
            });

            if let Some(callee) = cmp.mt.borrow(callee).expr.try_cast::<Symbol>(cmp.mt) {
                if cmp.mt.borrow(callee).name() == "define" {
                    declare(cmp, unit_toplevel, cmp.mt.borrow(ls).cdr());
                } else if cmp.mt.borrow(callee).name() == "begin" {
                    declare_toplevel_begin(cmp, unit_toplevel, cmp.mt.borrow(ls).cdr());
                }
            }
        }
    }

    fn declare(cmp: &mut Compiler, unit_toplevel: &mut UnitToplevel, args: ORef) {
        let args = args.try_cast::<Pair>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });

        let definiend = cmp.mt.borrow(args).car();
        let definiend = definiend.try_cast::<Syntax>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        });
        let definiend = root!(cmp.mt, cmp.mt.borrow(definiend).expr.try_cast::<Symbol>(cmp.mt).unwrap_or_else(|| {
            todo!() // error
        }));

        unit_toplevel.insert(String::from(definiend.name()), definiend);
    }

    fn declare_toplevel_begin(cmp: &mut Compiler, unit_toplevel: &mut UnitToplevel, mut args: ORef) {
        while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
            declare_toplevel_form(cmp, unit_toplevel, cmp.mt.borrow(args_pair).car());

            args = cmp.mt.borrow(args_pair).cdr();
        }
    }

    fn analyze_toplevel_form(cmp: &mut Compiler, env: &Rc<Env>, form: HandleAny) -> anf::PosExpr {
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
                    return analyze_definition(cmp, env, args, pos);
                } else if cmp.mt.borrow(callee).name() == "begin" {
                    let args = root!(cmp.mt, cmp.mt.borrow(ls).cdr());
                    return analyze_toplevel_begin(cmp, env, args, pos);
                }
            }
        }

        analyze_expr(cmp, env, form)
    }

    fn analyze_definition(cmp: &mut Compiler, env: &Rc<Env>, args: HandleAny, pos: HandleAny) -> anf::PosExpr {
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
        let val_expr = analyze_expr(cmp, env, val_expr);

        PosExpr {expr: Define(definiend, boxed::Box::new(val_expr)), pos}
    }

    fn analyze_toplevel_begin(cmp: &mut Compiler, env: &Rc<Env>, mut args: HandleAny, pos: HandleAny) -> anf::PosExpr {
        let mut stmts = Vec::new();

        while let Some(args_pair) = args.clone().try_cast::<Pair>(cmp.mt) {
            let stmt = root!(cmp.mt, args_pair.car());
            stmts.push(analyze_toplevel_form(cmp, env, stmt));

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

    let env = {
        let mut unit_toplevel = HashMap::new();
        declare_toplevel_form(cmp, &mut unit_toplevel, expr.oref());
        Rc::new(Env::UnitToplevel(unit_toplevel))
    };
    let body = analyze_toplevel_form(cmp, &env, expr);
    let body_pos = body.pos.clone();
    PosExpr {expr: r#Fn(None, anf::LiveVars::new(), vec![Id::fresh(cmp)], false, boxed::Box::new(body)), pos: body_pos}
}
