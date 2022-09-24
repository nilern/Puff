use crate::bytecode::{self, Bytecode};
use crate::oref::Gc;
use crate::handle::{Handle, HandleT};
use crate::list::Pair;
use crate::mutator::Mutator;
use crate::symbol::Symbol;

pub fn compile(mt: &mut Mutator, expr: Handle) -> Gc<Bytecode> {
    let mut builder = bytecode::Builder::new();

    if let Ok(expr) = HandleT::<()>::try_from(expr.clone()) {
        if let Some(_) = expr.try_cast::<Symbol>(mt) {
            todo!();
        } else if let Some(_) = expr.try_cast::<Pair>(mt) {
            todo!();
        }
    }

    builder.r#const(expr);
    builder.ret();

    return builder.build(mt);
}
