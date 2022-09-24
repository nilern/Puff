use crate::bytecode::{self, Bytecode};
use crate::oref::{ORef, Gc};
use crate::list::{Pair, EmptyList};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::heap_obj::Singleton;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let mut builder = bytecode::Builder::new();

    emit(mt, &mut builder, true, expr);

    return builder.build(mt);
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
