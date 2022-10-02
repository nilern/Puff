use crate::mutator::Mutator;
use crate::bytecode::{Opcode, decode_prune_mask};
use crate::oref::ORef;
use crate::heap_obj::Indexed;
use crate::closure::Closure;

pub fn run(mt: &mut Mutator) -> ORef {
    let mut ip = 0;

    macro_rules! tailcall {
        ($argc: ident) => {
            let callee = mt.regs()[mt.regs().len() - $argc];
            if let Some(callee) = callee.try_cast::<Closure>(mt) {
                // Pass arguments:
                mt.regs_enter($argc);

                // Jump:
                unsafe { mt.set_code(callee.as_ref().code); }
                ip = 0;

                // TODO: Ensure register space, reclaim garbage regs prefix and extend regs if necessary
                // TODO: GC safepoint (only becomes necessary with multithreading)

                // Check arity:
                // TODO: Varargs
                if $argc != unsafe { mt.code().as_ref().arity } {
                    todo!()
                }
            } else {
                todo!()
            }
        }
    }

    {
        let argc = mt.regs().len();
        assert!(argc > 0);
        tailcall!(argc);
    }

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

                Opcode::Local => {
                    let i = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    ip += 1;

                    mt.push(mt.regs()[i]);
                },

                Opcode::Clover => {
                    let i = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    ip += 1;

                    mt.push(unsafe { mt.regs()[0].unchecked_cast::<Closure>().as_ref().clovers()[i] });
                },

                Opcode::PopNNT => {
                    let n = unsafe { mt.code().as_ref().instrs()[ip] };
                    ip += 1;

                    mt.popnnt(n as usize);
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

                Opcode::Fn => {
                    let len = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    ip += 1;

                    let closure = Closure::new(mt, len);
                    mt.push(closure.into());
                    mt.popnnt(1 + len);
                },

                Opcode::Call => {
                    let argc = unsafe { mt.code().as_ref().instrs()[ip] } as usize;
                    ip += 1;

                    // Push stack frame:
                    let regs_len = mt.regs().len();
                    let mut frame_len = 0;
                    let mut mask_len = 0;
                    unsafe {
                        for (reg, prune) in decode_prune_mask(&mt.code().as_ref().instrs()[ip..]).enumerate() {
                            if !prune && reg < regs_len {
                                mt.save(reg);
                                frame_len += 1;
                            }

                            if reg % 7 == 0 {
                                mask_len += 1;
                            }
                        }
                    }
                    mt.finish_frame(frame_len, ip + mask_len);

                    tailcall!(argc);
                },

                Opcode::TailCall => {
                    let argc = unsafe { mt.code().as_ref().instrs()[ip] } as usize;

                    tailcall!(argc);
                },

                Opcode::Ret =>
                    if mt.stack_len() > 0 {
                        // TODO: Ensure register space, reclaim garbage regs prefix and extend regs if necessary

                        let v = mt.pop();

                        // Restore registers:
                        let (frame_len, rip) = mt.pop_frame();

                        // Push return value:
                        mt.push(v);

                        let f = mt.regs()[mt.regs().len() - frame_len - 1];
                        if let Some(f) = f.try_cast::<Closure>(mt) {
                            // Jump back:
                            unsafe { mt.set_code(f.as_ref().code); }
                            ip = rip;
                        } else {
                            todo!()
                        }
                    } else {
                        let res = mt.regs()[mt.regs().len() - 1];
                        mt.regs_enter(0);
                        return res;
                    }
            }
        } else {
            todo!()
        }
    }
}
