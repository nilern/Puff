use crate::mutator::Mutator;
use crate::bytecode::Opcode;
use crate::oref::ORef;
use crate::heap_obj::Indexed;
use crate::closure::Closure;

pub fn run(mt: &mut Mutator) -> ORef {
    macro_rules! tailcall {
        ($argc: ident) => {
            let callee = mt.regs()[mt.regs().len() - $argc];
            if let Some(callee) = callee.try_cast::<Closure>(mt) {
                // Pass arguments:
                mt.regs_enter($argc);

                // Jump:
                unsafe { mt.set_code(callee.as_ref().code); }

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
        if let Ok(op) = mt.next_opcode() {
            match op {
                Opcode::Const => {
                    let i = mt.next_oparg();

                    unsafe {
                        let c = mt.consts().as_ref().indexed_field()[i];
                        mt.push(c);
                    }
                },

                Opcode::Local => {
                    let i = mt.next_oparg();

                    mt.push(mt.regs()[i]);
                },

                Opcode::Clover => {
                    let i = mt.next_oparg();

                    mt.push(unsafe { mt.regs()[0].unchecked_cast::<Closure>().as_ref().clovers()[i] });
                },

                Opcode::PopNNT => {
                    let n = mt.next_oparg();

                    mt.popnnt(n as usize);
                },

                Opcode::Prune => mt.prune(),

                Opcode::Brf => {
                    let d = mt.peek_oparg();
                    
                    if mt.pop().is_truthy(mt) {
                        mt.branch_relative(1);
                    } else {
                        mt.branch_relative(d);
                    }
                },

                Opcode::Br => {
                    let d = mt.peek_oparg();
                    
                    mt.branch_relative(d);
                },

                Opcode::Fn => {
                    let len = mt.next_oparg();

                    let closure = Closure::new(mt, len);
                    mt.push(closure.into());
                    mt.popnnt(1 + len);
                },

                Opcode::Call => {
                    let argc = mt.next_oparg();

                    mt.push_frame();
                    tailcall!(argc);
                },

                Opcode::TailCall => {
                    let argc = mt.peek_oparg();

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
                            mt.branch_relative(rip);
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
