use std::alloc::{Layout, alloc, dealloc};
use std::mem::{size_of, align_of, transmute, swap};
use std::ptr::{self, NonNull};
use std::iter;

use crate::oref::{Gc, ORef};
use crate::r#type::{NonIndexedType, IndexedType, Type, Field};
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

    fn size(&self) -> usize { self.end as usize - self.start as usize }

    fn contains<T>(&self, ptr: *const T) -> bool {
        let addr = ptr as usize;
        self.start as usize <= addr && addr <= self.end as usize
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
    free: *mut u8,
    starts: Vec<bool>, // OPTIMIZE: Use a bitmap
    scan: *mut u8
}

impl Heap {
    pub fn new(size: usize) -> Self {
        let semi_size = size / 2;

        let tospace = Semispace::new(semi_size);
        let free = tospace.end;
        Self {
            fromspace: Semispace::new(semi_size),
            tospace,
            free,
            starts: iter::repeat(false).take(semi_size / size_of::<Granule>()).collect(),
            scan: free
        }
    }

    pub unsafe fn alloc_raw(&mut self, layout: Layout, indexed: bool) -> Option<NonNull<u8>> {
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
        let layout = self.borrow(r#type).layout();

        self.alloc_raw(layout, false).map(|obj| {
            // Initialize:
            let header = (obj.as_ptr() as *mut Header).offset(-1);
            header.write(Header::new(r#type.into()));
            ptr::write_bytes(obj.as_ptr(), 0, layout.size());

            obj
        })
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize) -> Option<NonNull<u8>> {
        let layout = self.borrow(r#type).layout(len);

        self.alloc_raw(layout, true).map(|obj| {
            // Initialize:
            Header::initialize_indexed(obj, Header::new(r#type.into()), len);
            ptr::write_bytes(obj.as_ptr(), 0, layout.size());

            obj
        })
    }

    unsafe fn shallow_copy<T>(&mut self, obj: Gc<T>) -> Option<Gc<T>> {
        let r#type = obj.r#type();

        if !self.borrow(r#type).has_indexed {
            let r#type = r#type.unchecked_cast::<NonIndexedType>();
            let layout = self.borrow(r#type).layout();

            self.alloc_raw(layout, false).map(|nptr| {
                // Initialize:
                let header = (nptr.as_ptr() as *mut Header).offset(-1);
                header.write(Header::new(r#type.into()));
                nptr.as_ptr().copy_from_nonoverlapping(obj.as_ptr() as *const u8, layout.size());

                Gc::<T>::new_unchecked(nptr.cast::<T>())
            })
        } else {
            let r#type = r#type.unchecked_cast::<IndexedType>();
            let len = *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1);
            let layout = self.borrow(r#type).layout(len);

            self.alloc_raw(layout, true).map(|nptr| {
                // Initialize:
                Header::initialize_indexed(nptr, Header::new(r#type.into()), len);
                nptr.as_ptr().copy_from_nonoverlapping(obj.as_ptr() as *const u8, layout.size());

                Gc::<T>::new_unchecked(nptr.cast::<T>())
            })
        }
    }

    /// Deref `oref`. This is safe because allocation and thus GC is impossible while a `&Heap` exists.
    /// Idea from the Josephine SpiderMonkey bindings (although I had the same intuition before hearing about that).
    pub fn borrow<'a, T>(&'a self, oref: Gc<T>) -> &'a T { unsafe { transmute(oref.as_ref()) } }

    pub unsafe fn flip(&mut self) {
        swap(&mut self.fromspace, &mut self.tospace);
        self.free = self.tospace.end;
        self.starts.fill(false);
        self.scan = self.free;
    }

    pub unsafe fn collect(&mut self) {
        while let Some(obj) = self.next_grey() {
            self.scan_header(obj);
            self.scan_fields(obj.r#type(), obj.as_ptr() as *mut u8);
        }
    }

    unsafe fn next_grey(&mut self) -> Option<Gc<()>> {
        // When self.scan <= self.free granule_index is computed redundantly -- but only once per collection:
        let mut granule_index = (self.scan as usize - self.tospace.start as usize) / size_of::<Granule>();
        while self.scan > self.free {
            granule_index -= 1;
            self.scan = (self.tospace.start as *mut Granule).add(granule_index) as *mut u8;

            if self.starts[granule_index] {
                return Some(Gc::new_unchecked(NonNull::new_unchecked(self.scan as *mut ())));
            }
        }

        None
    }

    pub unsafe fn mark_inplace(&mut self, field: *mut ORef) {
        if let Ok(obj) = Gc::<()>::try_from(*field) {
            *field = self.mark(obj).into();
        }
    }

    unsafe fn mark<T>(&mut self, obj: Gc<T>) -> Gc<T> {
        match obj.forwarding_address() {
            None => {
                let copy = self.shallow_copy(obj).unwrap().into();
                obj.set_forwarding_address(copy);
                let granule_index =
                    (copy.as_ptr() as usize - self.tospace.start as usize) / size_of::<Granule>();
                self.starts[granule_index] = true;
                copy
            },

            Some(copy) => copy
        }
    }

    unsafe fn scan_header(&mut self, obj: Gc<()>) {
        let header = obj.header_mut();
        (*header).set_type(self.mark((*header).r#type()));
    }

    // TODO: Avoid scan_field & scan_fields mutual recursion (although it is unlikely to be very deep):
    unsafe fn scan_field(&mut self, r#type: Gc<Type>, data: *mut u8) {
        if !self.borrow(r#type).inlineable {
            self.mark_inplace(data as *mut ORef);
        } else {
            self.scan_fields(r#type, data);
        }
    }

    unsafe fn scan_fields(&mut self, r#type: Gc<Type>, data: *mut u8) {
        if !self.borrow(r#type).is_bits {
            if !self.borrow(r#type).has_indexed {
                for field in r#type.as_ref().fields() {
                    self.scan_field(field.r#type, data.add(field.offset));
                }
            } else {
                let fields = r#type.as_ref().fields();
                let last_index = fields.len() - 1; // At least the indexed field, so len() > 0

                for field in &fields[0..last_index] {
                    self.scan_field(field.r#type, data.add(field.offset));
                }

                let indexed_field = fields[last_index];
                let indexed_field_type = indexed_field.r#type;
                if !self.borrow(indexed_field_type).is_bits {
                    let stride = self.borrow(indexed_field_type.unchecked_cast::<NonIndexedType>()).stride();
                    let len = *((data as *const Header).offset(-1) as *const usize).offset(-1);
                    let mut data = data.add(indexed_field.offset);
                    for _ in 0..len {
                        self.scan_field(indexed_field_type, data);
                        data = data.add(stride);
                    }
                }
            }
        }
    }

    pub unsafe fn zero_fromspace(&mut self) {
        self.fromspace.start.write_bytes(0, self.fromspace.size());
    }

    pub unsafe fn verify(&mut self) -> Result<(), VerificationError> {
        self.scan = self.tospace.end; // HACK: Using self.scan instead of an external iterator
        let mut next_obj = None;
        while let Some(obj) = self.next_grey() {
            self.verify_tospace_object(obj, next_obj)?;
            next_obj = Some(obj);
        }

        Ok(())
    }

    pub unsafe fn verify_oref(&self, oref: ORef) -> Result<(), VerificationError> {
        match Gc::<()>::try_from(oref) {
            Ok(obj) if !self.tospace.contains(obj.as_ptr()) => Err(VerificationError::ObjectNotInTospace {
                obj, tospace_start: self.tospace.start, tospace_end: self.tospace.end
            }),
            _ => Ok(())
        }
    }

    unsafe fn verify_tospace_object(&self, obj: Gc<()>, next_obj: Option<Gc<()>>) -> Result<(), VerificationError> {
        let r#type = obj.r#type();

        let data = obj.as_ptr() as *const u8;
        if data as usize % self.borrow(r#type).align != 0 {
            return Err(VerificationError::MisalignedObject);
        }
        match next_obj {
            Some(next_obj) => {
                let next_obj_header_start = if !self.borrow(next_obj.r#type()).has_indexed {
                    (next_obj.as_ptr() as *const Header).offset(-1) as usize
                } else {
                    ((next_obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) as usize
                };

                if data.add(obj.size()) as usize > next_obj_header_start {
                    return Err(VerificationError::OverlappingObjects);
                }
            },

            None =>
                if !self.tospace.contains(data.add(obj.size())) {
                    return Err(VerificationError::TospaceOverrun);
                }
        }

        if !self.tospace.contains(r#type.as_ptr()) {
            return Err(VerificationError::TypeNotInTospace);
        }

        self.verify_fields(obj, self.borrow(r#type), data)
    }

    unsafe fn verify_fields(&self, obj: Gc<()>, r#type: &Type, data: *const u8) -> Result<(), VerificationError> {
        if !r#type.is_bits {
            if !r#type.has_indexed {
                for field_descr in r#type.fields() {
                    self.verify_field(obj, field_descr, data.add(field_descr.offset))?;
                }

                Ok(())
            } else {
                let field_descrs = r#type.fields();
                let last_index = field_descrs.len() - 1; // At least the indexed field, so len() > 0

                for field_descr in &field_descrs[0..last_index] {
                    self.verify_field(obj, field_descr, data.add(field_descr.offset))?;
                }

                let indexed_field_descr = &field_descrs[last_index];
                let indexed_field_type = indexed_field_descr.r#type;
                if !self.borrow(indexed_field_type).is_bits {
                    let stride = self.borrow(indexed_field_type.unchecked_cast::<NonIndexedType>()).stride();
                    let len = *((data as *const Header).offset(-1) as *const usize).offset(-1);
                    let mut data = data.add(indexed_field_descr.offset);
                    for _ in 0..len {
                        self.verify_field(obj, indexed_field_descr, data)?;
                        data = data.add(stride);
                    }
                }

                Ok(())
            }
        } else {
            Ok(())
        }
    }

    unsafe fn verify_field(&self, obj: Gc<()>, descr: &Field<Type>, data: *const u8) -> Result<(), VerificationError> {
        let obj_size = obj.size();

        if descr.offset > obj_size {
            return Err(VerificationError::FieldOffsetBounds);
        }

        if descr.offset % self.borrow(descr.r#type).align != 0 {
            return Err(VerificationError::MisalignedField);
        }

        let field_size = if !self.borrow(descr.r#type).inlineable {
            size_of::<ORef>()
        } else {
            self.borrow(descr.r#type).min_size
        };
        let field_end = descr.offset + field_size;
        if field_end > obj_size {
            return Err(VerificationError::ObjectOverrun {obj_size, field_end});
        }

        if !self.tospace.contains(descr.r#type.as_ptr()) {
            return Err(VerificationError::FieldTypeNotInTospace);
        }

        if !self.borrow(descr.r#type).inlineable {
            self.verify_oref(*(data as *const ORef))?;
        } else {
            self.verify_fields(obj, self.borrow(descr.r#type), data)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum VerificationError {
    TypeNotInTospace,

    MisalignedObject,
    OverlappingObjects,
    TospaceOverrun,

    FieldTypeNotInTospace,
    FieldOffsetBounds,
    MisalignedField,
    ObjectOverrun {obj_size: usize, field_end: usize},
    ObjectNotInTospace {obj: Gc<()>, tospace_start: *const u8, tospace_end: *const u8}
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
            ).unchecked_cast::<NonIndexedType>();

            let v = heap.alloc_nonindexed(r#type);

            assert!(v.is_some());
            let obj = Gc::new_unchecked(v.unwrap().cast::<usize>());
            assert_eq!(obj.r#type(), r#type.into());
            assert!(obj.forwarding_address().is_none());
            assert_eq!(*heap.borrow(obj), 0usize);
        }
    }
}
