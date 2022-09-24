use crate::bytecode::{self, Bytecode};
use crate::oref::{ORef, Gc, WithinMt};
use crate::handle::{Handle, HandleT};
use crate::list::{Pair, EmptyList};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::heap_obj::Singleton;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let mut builder = bytecode::Builder::new();

    emit(mt, &mut builder, true, expr);

    return builder.build(mt);
}

mod anf {
    use super::*;

    pub enum Expr {
        If(Box<Block>, Box<Block>, Box<Block>),
        Const(Handle)
    }

    enum Stmt {
        Def(HandleT<Symbol>, Expr),
        Expr(Expr)
    }

    pub struct Block {
        stmts: Vec<Stmt>,
        body: Expr
    }

    pub struct BlockBuilder {
        stmts: Vec<Stmt>
    }

    impl BlockBuilder {
        pub fn new() -> Self { Self {stmts: Vec::new()} }

        pub fn build(self, body: Expr) -> Block {
            Block {
                stmts: self.stmts,
                body
            }
        }
    }
}

fn analyze(mt: &mut Mutator, expr: ORef) -> anf::Block {
    fn analyze_expr(mt: &mut Mutator, block: &mut anf::BlockBuilder,
        expr: ORef
    ) -> anf::Expr {
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

        anf::Expr::Const(mt.root(expr))
    }

    fn analyze_if(mt: &mut Mutator, args: ORef) -> anf::Expr {
        if let Some(args) = args.try_cast::<Pair>(mt) {
            let cond = analyze_block(mt, unsafe { args.as_ref().car });

            let branches = unsafe { args.as_ref().cdr };
            if let Some(branches) = branches.try_cast::<Pair>(mt) {
                let conseq = analyze_block(mt, unsafe { branches.as_ref().car });

                let branches = unsafe { branches.as_ref().cdr };
                if let Some(branches) = branches.try_cast::<Pair>(mt) {
                    let alt = analyze_block(mt, unsafe { branches.as_ref().car });

                    let branches = unsafe { branches.as_ref().cdr };
                    if branches == EmptyList::instance(mt).into() {
                        anf::Expr::If(
                            Box::new(cond),
                            Box::new(conseq),
                            Box::new(alt)
                        )
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

    fn analyze_block(mt: &mut Mutator, expr: ORef) -> anf::Block {
        let mut block = anf::BlockBuilder::new();
        let body = analyze_expr(mt, &mut block, expr);
        block.build(body)
    }

    analyze_block(mt, expr)
}

fn emit(mt: &mut Mutator, builder: &mut bytecode::Builder, tail: bool,
    expr: ORef)
{
    if let Ok(expr) = Gc::<()>::try_from(expr.clone()) {
        if let Some(_) = expr.try_cast::<Symbol>(mt) {
            todo!();
        } else if let Some(ls) = expr.try_cast::<Pair>(mt) {
            if let Ok(callee) = Gc::<()>::try_from(unsafe { ls.as_ref().car }) {
                if let Some(callee) = callee.try_cast::<Symbol>(mt) {
                    if unsafe { callee.as_ref().name() == "if" } {
                        let args = unsafe { ls.as_ref().cdr };
                        return emit_if(mt, builder, tail, args);
                    }
                }
            }

            todo!()
        }
    }

    builder.r#const(mt, expr);
    if tail { builder.ret(); }
}

fn emit_if(mt: &mut Mutator, builder: &mut bytecode::Builder, tail: bool,
    args: ORef)
{
    if let Some(args) = args.try_cast::<Pair>(mt) {
        emit(mt, builder, false, unsafe { args.as_ref().car });
        let brf_i = builder.brf();

        let branches = unsafe { args.as_ref().cdr };
        if let Some(branches) = branches.try_cast::<Pair>(mt) {
            emit(mt, builder, tail, unsafe { branches.as_ref().car });
            let br_i = if tail { None } else { Some(builder.br()) };

            builder.backpatch(brf_i);

            let branches = unsafe { branches.as_ref().cdr };
            if let Some(branches) = branches.try_cast::<Pair>(mt) {
                emit(mt, builder, tail, unsafe { branches.as_ref().car });

                if let Some(br_i) = br_i { builder.backpatch(br_i); }

                let branches = unsafe { branches.as_ref().cdr };
                if branches != EmptyList::instance(mt).into() {
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
