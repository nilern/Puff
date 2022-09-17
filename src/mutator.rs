use std::ptr::NonNull;
use std::mem::{align_of, size_of};
use std::alloc::Layout;

use crate::heap::Heap;
use crate::oref::{AsType, ORef, Gc};
use crate::r#type::{Type, Field, IndexedType, NonIndexedType, BitsType};
use crate::symbol::{Symbol, SymbolTable};
use crate::heap_obj::{Indexed, Header, min_size_of_indexed, align_of_indexed};
use crate::handle::{Handle, HandlePool};

const USIZE_TYPE_SIZE: usize = min_size_of_indexed::<Type>();

const USIZE_TYPE_LAYOUT: Layout = unsafe {
    Layout::from_size_align_unchecked(
        USIZE_TYPE_SIZE, align_of_indexed::<Type>()
    )
};

pub struct Types {
    pub r#type: Gc<IndexedType>,
    pub symbol: Gc<IndexedType>
}

pub struct Mutator {
    heap: Heap,
    handles: HandlePool,
    types: Types,
    symbols: SymbolTable
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

            // -----------------------------------------------------------------

            Some(Self {
                heap,
                handles: HandlePool::new(),
                types: Types { r#type, symbol },
                symbols: SymbolTable::new()
            })
        }
    }

    pub fn types(&self) -> &Types { &self.types }

    // HACK: returns raw pointer because of lifetime issues in Symbol::new:
    pub fn symbols_mut(&mut self) -> *mut SymbolTable { &mut self.symbols as _ }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize)
        -> Option<NonNull<u8>>
    {
        self.heap.alloc_indexed(r#type, len)
    }

    pub unsafe fn root(&mut self, oref: ORef) -> Handle {
        self.handles.root(oref)
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
