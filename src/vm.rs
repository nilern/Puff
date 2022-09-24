use crate::mutator::Mutator;
use crate::bytecode::{Bytecode, Opcode};
use crate::handle::HandleT;
use crate::oref::ORef;
use crate::heap_obj::Indexed;

pub fn run(mt: &mut Mutator, code: HandleT<Bytecode>) -> ORef {
    mt.set_code(code);
    let mut ip = 0;

    loop {
        let op = unsafe { mt.code().as_ref().instrs()[ip] };
        if let Ok(op) = Opcode::try_from(op) {
            ip += 1;

            match op {
                Opcode::Const => {
                    let i = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    ip += 1;

                    unsafe {
                        let c = mt.consts().as_ref().indexed_field()[i];
                        mt.push(c);
                    }
                },

                Opcode::Brf => {
                    let d = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    
                    if mt.pop().is_truthy(mt) {
                        ip += 1;
                    } else {
                        ip += d;
                    }
                },

                Opcode::Br => {
                    let d = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    
                    ip += d;
                },

                Opcode::Ret => return mt.regs()[mt.regs().len() - 1]
            }
        } else {
            todo!()
        }
    }
}
