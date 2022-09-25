use crate::bytecode::{self, Bytecode};
use crate::oref::{ORef, Gc};
use crate::handle::Handle;
use crate::list::{Pair, EmptyList};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::heap_obj::Singleton;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let anf = analyze(mt, expr);

    emit(mt, &anf)
}

mod anf {
    use super::*;

    pub enum Triv {
        Const(Handle)
    }

    pub enum Expr {
        If(Box<Expr>, Box<Expr>, Box<Expr>),
        Triv(Triv)
    }
}

fn analyze(mt: &mut Mutator, expr: ORef) -> anf::Expr {
    use anf::Expr::*;
    use anf::Triv::*;

    fn analyze_expr(mt: &mut Mutator, expr: ORef) -> anf::Expr {
        if let Ok(expr) = Gc::<()>::try_from(expr.clone()) {
            if let Some(_) = expr.try_cast::<Symbol>(mt) {
                todo!();
            } else if let Some(ls) = expr.try_cast::<Pair>(mt) {
                let callee = unsafe { ls.as_ref().car };
                if let Ok(callee) = Gc::<()>::try_from(callee) {
                    if let Some(callee) = callee.try_cast::<Symbol>(mt) {
                        if unsafe { callee.as_ref().name() == "if" } {
                            let args = unsafe { ls.as_ref().cdr };
                            return analyze_if(mt, args);
                        }
                    }
                }

                todo!()
            }
        }

        Triv(Const(mt.root(expr)))
    }

    fn analyze_if(mt: &mut Mutator, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(mt) {
            let cond = analyze_expr(mt, unsafe { args.as_ref().car });

            let branches = unsafe { args.as_ref().cdr };
            if let Some(branches) = branches.try_cast::<Pair>(mt) {
                let conseq = analyze_expr(mt, unsafe { branches.as_ref().car });

                let branches = unsafe { branches.as_ref().cdr };
                if let Some(branches) = branches.try_cast::<Pair>(mt) {
                    let alt = analyze_expr(mt, unsafe { branches.as_ref().car });

                    let branches = unsafe { branches.as_ref().cdr };
                    if branches == EmptyList::instance(mt).into() {
                        If(Box::new(cond), Box::new(conseq), Box::new(alt))
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

    analyze_expr(mt, expr)
}

fn emit(mt: &mut Mutator, expr: &anf::Expr) -> Gc<Bytecode> {
    use anf::Expr::*;
    use anf::Triv::*;

    fn emit_expr(mt: &mut Mutator, builder: &mut bytecode::Builder, tail: bool,
        expr: &anf::Expr)
    {
        match *expr {
            If(ref cond, ref conseq, ref alt) => {
                emit_expr(mt, builder, false, cond);
                let brf_i = builder.brf();

                emit_expr(mt, builder, tail, conseq);
                let br_i = if tail { None } else { Some(builder.br()) };

                builder.backpatch(brf_i);

                emit_expr(mt, builder, tail, alt);

                if let Some(br_i) = br_i { builder.backpatch(br_i); }
            },

            Triv(Const(ref c)) => {
                builder.r#const(mt, **c);
                if tail { builder.ret(); }
            }
        }
    }

    let mut builder = bytecode::Builder::new();

    emit_expr(mt, &mut builder, true, expr);

    builder.build(mt)
}
