use std::ptr::NonNull;
use std::mem::{align_of, size_of};
use std::alloc::Layout;

use crate::heap::Heap;
use crate::oref::{AsType, ORef, Gc, Fixnum};
use crate::r#type::{Type, Field, IndexedType, NonIndexedType, BitsType};
use crate::symbol::{Symbol, SymbolTable};
use crate::heap_obj::{NonIndexed, Indexed, Header, min_size_of_indexed,
    align_of_indexed};
use crate::handle::{Handle, HandleT, HandlePool};
use crate::list::{EmptyList, Pair};
use crate::bytecode::{Opcode, Bytecode, decode_prune_mask};
use crate::array::Array;
use crate::closure::Closure;
use crate::regs::Regs;

const USIZE_TYPE_SIZE: usize = min_size_of_indexed::<Type>();

const USIZE_TYPE_LAYOUT: Layout = unsafe {
    Layout::from_size_align_unchecked(
        USIZE_TYPE_SIZE, align_of_indexed::<Type>()
    )
};

pub struct Types {
    pub r#type: Gc<IndexedType>,
    pub symbol: Gc<IndexedType>,
    pub pair: Gc<NonIndexedType>,
    pub empty_list: Gc<NonIndexedType>,
    pub array_of_any: Gc<IndexedType>,
    pub bytecode: Gc<IndexedType>,
    pub closure: Gc<IndexedType>
}

pub struct Singletons {
    pub empty_list: Gc<EmptyList>
}

pub struct Mutator {
    heap: Heap,
    handles: HandlePool,

    types: Types,
    singletons: Singletons,
    symbols: SymbolTable,

    code: Option<Gc<Bytecode>>,
    consts: Option<Gc<Array<ORef>>>,
    pc: usize,
    regs: Regs,
    stack: Vec<ORef>
}

impl Mutator {
    pub fn new(heap_size: usize) -> Option<Self> {
        let mut heap = Heap::new(heap_size);

        unsafe {
            // Create strongly connected bootstrap types, Fields uninitialized:
            // -----------------------------------------------------------------

            let r#type_nptr = heap.alloc_raw(Type::TYPE_LAYOUT, true)?;
            let mut r#type =
                Gc::new_unchecked(r#type_nptr.cast::<IndexedType>());
            Header::initialize_indexed(r#type_nptr,
                Header::new(r#type.as_type()), Type::TYPE_LEN);
            *r#type.as_mut() = IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Type>(),
                align: align_of_indexed::<Type>()
            });

            let field_type_nptr = heap.alloc_raw(Field::<()>::TYPE_LAYOUT,
                true)?;
            let mut field_type =
                Gc::new_unchecked(r#field_type_nptr.cast::<NonIndexedType>());
            Header::initialize_indexed(r#field_type_nptr,
                Header::new(r#type.as_type()), Field::<()>::TYPE_LEN);
            *field_type.as_mut() = NonIndexedType::new_unchecked(Type {
                min_size: size_of::<Field::<()>>(),
                align: align_of::<Field::<()>>()
            });

            let usize_type_nptr = heap.alloc_raw(USIZE_TYPE_LAYOUT, true)?;
            let mut usize_type =
                Gc::new_unchecked(r#usize_type_nptr.cast::<BitsType>());
            Header::initialize_indexed(r#usize_type_nptr,
                Header::new(r#type.as_type()), 0);
            *usize_type.as_mut() = BitsType::from_static::<usize>();

            // Initialize Fields of strongly connected bootstrap types:
            // -----------------------------------------------------------------

            field_type.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: r#type.as_type(), offset: 0 },
                Field {
                    r#type: usize_type.as_type(),
                    offset: size_of::<Gc<()>>()
                }
            ]);

            r#type.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: usize_type.as_type(), offset: 0 },
                Field {
                    r#type: usize_type.as_type(),
                    offset: size_of::<usize>()
                },
                Field {
                    r#type: field_type.as_type(),
                    offset: min_size_of_indexed::<Type>()
                }
            ]);

            // Create other `.types`:
            // -----------------------------------------------------------------

            let mut any = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, 0
            )?);
            *any.as_mut() = Type {
                min_size: size_of::<ORef>(),
                align: align_of::<ORef>()
            };

            let mut u8_type = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, 0
            )?);
            *u8_type.as_mut() = BitsType::from_static::<u8>();

            let mut symbol = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, Symbol::TYPE_LEN
            )?);
            *symbol.as_mut() = IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Symbol>(),
                align: align_of_indexed::<Symbol>()
            });
            symbol.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: usize_type.as_type(), offset: 0 },
                Field {
                    r#type: u8_type.as_type(),
                    offset: size_of::<usize>()
                }
            ]);

            let mut pair = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, Pair::TYPE_LEN
            )?);
            *pair.as_mut() = NonIndexedType::from_static::<Pair>();
            pair.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: any, offset: 0 },
                Field {
                    r#type: any,
                    offset: size_of::<Gc<()>>()
                }
            ]);

            let mut empty_list = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, EmptyList::TYPE_LEN
            )?);
            *empty_list.as_mut() = NonIndexedType::from_static::<EmptyList>();

            let mut array_of_any = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, Array::<ORef>::TYPE_LEN
            )?);
            *array_of_any.as_mut() = IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Array::<ORef>>(),
                align: align_of_indexed::<Array::<ORef>>()
            });
            array_of_any.as_type().as_mut().indexed_field_mut()
                .copy_from_slice(&[
                    Field { r#type: any, offset: 0 }
                ]);

            let mut bytecode = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, Bytecode::TYPE_LEN
            )?);
            *bytecode.as_mut() = IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Bytecode>(),
                align: align_of_indexed::<Bytecode>()
            });
            bytecode.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: usize_type.as_type(), offset: 0 },
                Field { r#type: usize_type.as_type(), offset: size_of::<usize>() },
                Field { r#type: array_of_any.as_type(), offset: 2 * size_of::<usize>() },
                Field {
                    r#type: u8_type.as_type(),
                    offset: min_size_of_indexed::<Bytecode>()
                }
            ]);

            let mut closure = Gc::new_unchecked(Type::bootstrap_new(
                &mut heap, r#type, Closure::TYPE_LEN
            )?);
            *closure.as_mut() = IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Closure>(),
                align: align_of_indexed::<Closure>()
            });
            closure.as_type().as_mut().indexed_field_mut().copy_from_slice(&[
                Field { r#type: bytecode.as_type(), offset: 0 },
                Field {
                    r#type: any,
                    offset: min_size_of_indexed::<Closure>()
                }
            ]);

            // Create singleton instances:
            // -----------------------------------------------------------------

            let empty_list_inst = Gc::new_unchecked(heap.alloc_nonindexed(
                empty_list
            )?.cast::<EmptyList>());

            // -----------------------------------------------------------------

            Some(Self {
                heap,
                handles: HandlePool::new(),

                types: Types { r#type, symbol, pair, empty_list,  bytecode, array_of_any, closure },
                singletons: Singletons { empty_list: empty_list_inst },
                symbols: SymbolTable::new(),

                code: None,
                consts: None,
                pc: 0,
                regs: Regs::with_capacity((1 << 20 /* 1 MiB */) / size_of::<ORef>()),
                stack: Vec::new()
            })
        }
    }

    pub fn types(&self) -> &Types { &self.types }

    pub fn singletons(&self) -> &Singletons { &self.singletons }

    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    // HACK: returns raw pointer because of lifetime issues in Symbol::new:
    pub fn symbols_mut(&mut self) -> *mut SymbolTable { &mut self.symbols as _ }

    fn set_code(&mut self, code: Gc<Bytecode>) {
        self.code = Some(code);
        unsafe { self.consts = Some(code.as_ref().consts); }
        self.pc = 0;
    }

    unsafe fn code(&self) -> Gc<Bytecode> { self.code.unwrap() }

    unsafe fn consts(&self) -> Gc<Array<ORef>> { self.consts.unwrap() }

    fn next_opcode(&mut self) -> Result<Opcode, ()> {
        let op = Opcode::try_from(unsafe { self.code().as_ref().instrs()[self.pc] });
        self.pc += 1;
        op
    }

    fn peek_oparg(&self) -> usize { unsafe { self.code().as_ref().instrs()[self.pc] as usize } }

    fn next_oparg(&mut self) -> usize {
        let arg = self.peek_oparg();
        self.pc += 1;
        arg
    }

    pub fn regs(&self) -> &Regs { &self.regs }

    pub fn push(&mut self, v: ORef) { self.regs.push(v); }

    pub fn pop(&mut self) -> ORef { self.regs.pop().unwrap() }

    pub fn dump_regs(&self) { self.regs.dump(self); }

    pub unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>) -> NonNull<u8> {
        self.heap.alloc_nonindexed(r#type).unwrap_or_else(||
            todo!() // Need to GC, then retry
        )
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize) -> NonNull<u8> {
        self.heap.alloc_indexed(r#type, len).unwrap_or_else(||
            todo!() // Need to GC, then retry
        )
    }

    pub unsafe fn alloc_static<T: NonIndexed>(&mut self) -> NonNull<T> {
        self.alloc_nonindexed(T::reify_nonindexed(self)).cast::<T>()
    }

    pub fn root(&mut self, oref: ORef) -> Handle {
        unsafe { self.handles.root(oref) }
    }

    pub fn root_t<T>(&mut self, obj: Gc<T>) -> HandleT<T> {
        unsafe { self.handles.root_t(obj) }
    }

    fn tailcall(&mut self, argc: usize) {
        let callee = self.regs[self.regs.len() - argc];
        if let Some(callee) = callee.try_cast::<Closure>(self) {
            let code = unsafe { callee.as_ref().code };

            // Pass arguments:
            self.regs.enter(argc);

            // Jump:
            self.set_code(code);

            // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
            self.regs.ensure(unsafe { code.as_ref().max_regs });

            // TODO: GC safepoint (only becomes necessary with multithreading)

            // Check arity:
            // TODO: Varargs
            if argc != unsafe { self.code().as_ref().arity } {
                todo!()
            }
        } else {
            todo!()
        }
    }

    pub fn invoke(&mut self) -> ORef {
        {
            let argc = self.regs.len();
            assert!(argc > 0);
            self.tailcall(argc);
        }

        loop {
            if let Ok(op) = self.next_opcode() {
                match op {
                    Opcode::Const => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.consts().as_ref().indexed_field()[i]); }
                    },

                    Opcode::Local => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.regs[i]); }
                    },

                    Opcode::Clover => {
                        let i = self.next_oparg();

                        unsafe {
                            self.regs.push_unchecked(self.regs[0].unchecked_cast::<Closure>().as_ref().clovers()[i]);
                        }
                    },

                    Opcode::PopNNT => {
                        let n = self.next_oparg();

                        self.regs.popnnt(n);
                    },

                    Opcode::Prune => {
                        let regs_len = self.regs.len();
                        let mut mask_len = 0;
                        let mut free_reg = 0;
                        unsafe {
                            for (reg, prune) in decode_prune_mask(&self.code().as_ref().instrs()[self.pc..]).enumerate()
                            {
                                if !prune && reg < regs_len {
                                    self.regs[free_reg] = self.regs[reg];
                                    free_reg += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }
                            }
                        }
                        self.regs.truncate(free_reg);

                        self.pc += mask_len;
                    },

                    Opcode::Box | Opcode::BoxSet | Opcode::BoxGet => todo!(),

                    Opcode::Brf =>
                        if self.regs.pop().unwrap().is_truthy(self) {
                            self.pc += 1;
                        } else {
                            let d = self.peek_oparg();

                            self.pc += d;
                        },

                    Opcode::Br => {
                        let d = self.peek_oparg();
                        
                        self.pc += d;
                    },

                    Opcode::Fn => {
                        let ci = self.next_oparg();
                        let len = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.consts().as_ref().indexed_field()[ci]); }
                        let closure = Closure::new(self, len);
                        self.regs.popn(len + 1);
                        unsafe { self.regs.push_unchecked(closure.into()); }
                    },

                    Opcode::Call => {
                        let argc = self.next_oparg();

                        let max_frame_len = self.regs.len() - argc;
                        let mut frame_len = 0;
                        let mut mask_len = 0;
                        unsafe {
                            for (reg, prune) in decode_prune_mask(&self.code().as_ref().instrs()[self.pc..]).enumerate()
                            {
                                if !prune && reg < max_frame_len {
                                    self.stack.push(self.regs[reg]);
                                    frame_len += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }
                            }
                        }

                        self.pc += mask_len;

                        self.stack.push(Fixnum::try_from(frame_len as isize).unwrap().into());
                        self.stack.push(Fixnum::try_from(self.pc as isize).unwrap().into());

                        self.tailcall(argc);
                    },

                    Opcode::TailCall => {
                        let argc = self.peek_oparg();

                        self.tailcall(argc);
                    },

                    Opcode::Ret =>
                        if self.stack.len() > 0 {
                            // Restore registers:

                            let rip =
                                unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };
                            let frame_len =
                                unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };

                            let start = self.stack.len() - frame_len;
                            self.regs.re_enter(&self.stack[start..]);
                            self.stack.truncate(start);

                            let f = self.regs[self.regs.len() - frame_len - 1];
                            if let Some(f) = f.try_cast::<Closure>(self) {
                                let code = unsafe { f.as_ref().code };

                                // Jump back:
                                self.set_code(code);
                                self.pc = rip;

                                // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                                self.regs.ensure(unsafe { code.as_ref().max_regs });
                            } else {
                                todo!()
                            }
                        } else {
                            let res = self.regs[self.regs.len() - 1];
                            self.regs.enter(0);
                            return res;
                        }
                }
            } else {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mutator_new() {
        Mutator::new(1 << 20 /* 1 MiB */);
    }
}
