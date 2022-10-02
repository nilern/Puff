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
use crate::bytecode::Bytecode;
use crate::array::Array;
use crate::closure::Closure;

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

struct Regs {
    regs: Vec<ORef>,
    start: usize
}

impl Regs {
    fn new() -> Self {
        Self {
            regs: Vec::new(),
            start: 0
        }
    }

    fn as_slice(&self) -> &[ORef] { &self.regs[self.start..] }

    fn pop(&mut self) -> Option<ORef> { self.regs.pop() }

    fn push(&mut self, v: ORef) { self.regs.push(v) }

    fn enter(&mut self, new_len: usize) {
        self.start = self.regs.len() - new_len;
    }

    fn popnnt(&mut self, n: usize) {
        let len = self.regs.len();
        let top = self.regs[len - 1];
        let new_len = len - n as usize;
        self.regs.truncate(new_len);
        self.regs[new_len - 1] = top;
    }
}

pub struct Mutator {
    heap: Heap,
    handles: HandlePool,

    types: Types,
    singletons: Singletons,
    symbols: SymbolTable,

    code: Option<Gc<Bytecode>>,
    consts: Option<Gc<Array<ORef>>>,
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
                Field { r#type: array_of_any.as_type(), offset: size_of::<usize>() },
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
                regs: Regs::new(),
                stack: Vec::new()
            })
        }
    }

    pub fn types(&self) -> &Types { &self.types }

    pub fn singletons(&self) -> &Singletons { &self.singletons }

    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    // HACK: returns raw pointer because of lifetime issues in Symbol::new:
    pub fn symbols_mut(&mut self) -> *mut SymbolTable { &mut self.symbols as _ }

    pub fn set_code(&mut self, code: Gc<Bytecode>) {
        self.code = Some(code);
        unsafe { self.consts = Some(code.as_ref().consts); }
    }

    pub unsafe fn code(&self) -> Gc<Bytecode> { self.code.unwrap() }

    pub unsafe fn consts(&self) -> Gc<Array<ORef>> { self.consts.unwrap() }

    pub fn regs(&self) -> &[ORef] { self.regs.as_slice() }

    pub fn regs_enter(&mut self, len: usize) { self.regs.enter(len); }

    pub fn push(&mut self, v: ORef) { self.regs.push(v); }

    pub fn pop(&mut self) -> ORef { self.regs.pop().unwrap() }

    pub fn popnnt(&mut self, n: usize) { self.regs.popnnt(n); }

    pub fn stack_len(&self) -> usize { self.stack.len() }

    pub fn save(&mut self, reg: usize) {
        self.stack.push(self.regs.as_slice()[reg]);
    }

    pub fn finish_frame(&mut self, len: usize, ip: usize) {
        self.stack.push(Fixnum::try_from(len as isize).unwrap().into());
        self.stack.push(Fixnum::try_from(ip as isize).unwrap().into());
    }

    pub fn pop_frame(&mut self) -> (usize, usize) {
        let ip = unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };
        let frame_len = unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };

        let start = self.stack.len() - frame_len;
        for &v in &self.stack[start..] {
            self.regs.push(v);
        }
        self.stack.truncate(start);

        (frame_len, ip)
    }

    pub unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>)
        -> Option<NonNull<u8>>
    {
        self.heap.alloc_nonindexed(r#type)
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize)
        -> Option<NonNull<u8>>
    {
        self.heap.alloc_indexed(r#type, len)
    }

    pub unsafe fn alloc_static<T: NonIndexed>(&mut self)
        -> Option<NonNull<T>>
    {
        self.heap.alloc_nonindexed(T::reify_nonindexed(self))
            .map(NonNull::cast::<T>)
    }

    pub fn root(&mut self, oref: ORef) -> Handle {
        unsafe { self.handles.root(oref) }
    }

    pub fn root_t<T>(&mut self, obj: Gc<T>) -> HandleT<T> {
        unsafe { self.handles.root_t(obj) }
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
