use std::alloc::{Layout, alloc, dealloc};
use std::mem::{align_of, transmute, swap};
use std::ptr::{self, NonNull};

use crate::oref::{AsType, Gc, ORef};
use crate::r#type::{NonIndexedType, IndexedType, Type};
use crate::heap_obj::Header;

struct Granule(usize);

struct Semispace {
    start: *mut u8,
    end: *mut u8
}

impl Semispace {
    const ALIGN: usize = align_of::<Granule>();

    fn new(mut size: usize) -> Self {
        size &= !(Self::ALIGN - 1);

        unsafe {
            let start =
                alloc(Layout::from_size_align_unchecked(size, Self::ALIGN));

            Self {
                start,
                end: start.add(size)
            }
        }
    }
}

impl Drop for Semispace {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start, Layout::from_size_align_unchecked(
                self.end as usize - self.start as usize,
                Self::ALIGN
            ));
        }
    }
}

pub struct Heap {
    fromspace: Semispace,
    tospace: Semispace,
    free: *mut u8
}

impl Heap {
    pub fn new(size: usize) -> Self {
        let semi_size = size / 2;

        let tospace = Semispace::new(semi_size);
        let free = tospace.end;
        Self {
            fromspace: Semispace::new(semi_size),
            tospace,
            free
        }
    }

    unsafe fn flip(&mut self) {
        swap(&mut self.fromspace, &mut self.tospace);
        self.free = self.tospace.end;
    }

    pub unsafe fn alloc_raw(&mut self, layout: Layout, indexed: bool)
        -> Option<NonNull<u8>>
    {
        let mut addr = self.free as usize;

        addr = addr.checked_sub(layout.size())?; // Bump by `size`
        
        let align = layout.align()
            .max(align_of::<Header>()); // Ensure header gets aligned
        addr &= !(align - 1); // Round down to `align`
        let obj = addr as *mut u8;

        let header = (obj as *mut Header).offset(-1);
        addr = header as usize;

        if indexed {
            addr = (header as *mut usize).offset(-1) as usize;
        }

        if addr >= self.tospace.start as usize {
            self.free = addr as *mut u8;

            Some(NonNull::new_unchecked(obj))
        } else {
            None
        }
    }

    pub unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>) -> Option<NonNull<u8>> {
        let layout = r#type.as_ref().layout();

        self.alloc_raw(layout, false).map(|obj| {
            // Initialize:
            let header = (obj.as_ptr() as *mut Header).offset(-1);
            header.write(Header::new(r#type.as_type()));
            ptr::write_bytes(obj.as_ptr(), 0, layout.size());

            obj
        })
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize) -> Option<NonNull<u8>> {
        let layout = r#type.as_ref().layout(len);

        self.alloc_raw(layout, true).map(|obj| {
            // Initialize:
            Header::initialize_indexed(obj, Header::new(r#type.as_type()), len);
            ptr::write_bytes(obj.as_ptr(), 0, layout.size());

            obj
        })
    }

    unsafe fn shallow_copy(&mut self, obj: Gc<()>) -> Option<Gc<()>> {
        let r#type = obj.r#type();

        if !r#type.as_ref().has_indexed {
            let r#type = r#type.unchecked_cast::<NonIndexedType>();
            let layout = r#type.as_ref().layout();

            self.alloc_raw(layout, false).map(|nptr| {
                // Initialize:
                let header = (nptr.as_ptr() as *mut Header).offset(-1);
                header.write(Header::new(r#type.as_type()));
                nptr.as_ptr().copy_from_nonoverlapping(obj.as_ptr() as *const u8, layout.size());

                Gc::<()>::new_unchecked(nptr.cast::<()>())
            })
        } else {
            let r#type = r#type.unchecked_cast::<IndexedType>();
            let len = *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1);
            let layout = r#type.as_ref().layout(len);

            self.alloc_raw(layout, true).map(|nptr| {
                // Initialize:
                Header::initialize_indexed(nptr, Header::new(r#type.as_type()), len);
                nptr.as_ptr().copy_from_nonoverlapping(obj.as_ptr() as *const u8, layout.size());

                Gc::<()>::new_unchecked(nptr.cast::<()>())
            })
        }
    }

    pub unsafe fn mark(&mut self, oref: usize) -> Option<Gc<()>> {
        if oref != 0 {
            let oref = transmute::<usize, ORef>(oref);

            if let Ok(obj) = Gc::<()>::try_from(oref) {
                return Some(match obj.forwarding_address() {
                    None => {
                        let copy = self.shallow_copy(obj).unwrap().into();
                        obj.set_forwarding_address(copy);
                        copy
                    },

                    Some(copy) => copy
                });
            }
        }

        None // Null (= uninitialized) or tagged scalar
    }

    // TODO: Avoid scan_field & scan_fields mutual recursion (although it is unlikely to be very deep):
    unsafe fn scan_field(&mut self, r#type: Gc<Type>, data: *mut u8) {
        if !r#type.as_ref().inlineable {
            let data = data as *mut usize;
            if let Some(copy) = self.mark(*data) {
                *data = transmute::<Gc<()>, usize>(copy);
            }
        } else {
            self.scan_fields(r#type, data);
        }
    }

    unsafe fn scan_fields(&mut self, r#type: Gc<Type>, data: *mut u8) {
        if !r#type.as_ref().is_bits {
            if !r#type.as_ref().has_indexed {
                for field in r#type.as_ref().fields() {
                    self.scan_field(field.r#type, data.add(field.offset));
                }
            } else {
                let fields = r#type.as_ref().fields();
                let last_index = fields.len() - 1; // At least the indexed field, so len() > 0

                for field in &fields[0..last_index] {
                    self.scan_field(field.r#type, data.add(field.offset));
                }

                let len = *((data as *const Header).offset(-1) as *const usize).offset(-1);
                let indexed_field = fields[last_index];
                let stride = indexed_field.r#type.unchecked_cast::<NonIndexedType>().as_ref().stride();
                let mut data = data.add(indexed_field.offset);
                for _ in 0..len {
                    self.scan_field(indexed_field.r#type, data);
                    data = data.add(stride);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::r#type::BitsType;

    #[test]
    fn heap_lifecycle() {
        Heap::new(1 << 20 /* 1 MiB */);
    }

    #[test]
    fn alloc_nonindexed() {
        let mut heap = Heap::new(1 << 20 /* 1 MiB */);

        unsafe {
            let mut r#type = BitsType::from_static::<usize>(true);
            let r#type = Gc::new_unchecked(
                NonNull::new_unchecked(&mut r#type as *mut BitsType)
            ).as_nonindexed();

            let v = heap.alloc_nonindexed(r#type);

            assert!(v.is_some());
            let obj = Gc::new_unchecked(v.unwrap().cast::<usize>());
            assert_eq!(obj.r#type(), r#type.as_type());
            assert!(obj.forwarding_address().is_none());
            assert_eq!(*obj.as_ref(), 0usize);
        }
    }
}
