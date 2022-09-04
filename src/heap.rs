use std::alloc::{Layout, alloc, dealloc};
use std::mem::align_of;
use std::ptr::{self, NonNull};

use super::oref::{Gc, Header};
use super::r#type::NonIndexedType;

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

struct Heap {
    fromspace: Semispace,
    tospace: Semispace,
    free: *mut u8
}

impl Heap {
    fn new(size: usize) -> Self {
        let semi_size = size / 2;

        let fromspace = Semispace::new(semi_size);
        let free = fromspace.end;
        Self {
            fromspace,
            tospace: Semispace::new(semi_size),
            free
        }
    }

    unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>)
        -> Option<NonNull<u8>>
    {
        let mut addr = self.free as usize;

        let size = r#type.as_ref().min_size();
        addr = addr.checked_sub(size)?; // Bump by `size`

        let mut align = r#type.as_ref().align();
        align = align.max(align_of::<Header>()); // Ensure header gets aligned
        addr &= !(align - 1); // Round down to `align`

        if addr >= self.fromspace.start as usize {
            let obj = addr as *mut u8;
            let header = (obj as *mut Header).offset(-1);

            // Initialize:
            header.write(Header::new(r#type.as_type()));
            ptr::write_bytes(obj, 0, size);

            self.free = header as *mut u8;

            Some(NonNull::new_unchecked(obj))
        } else {
            None
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
            let mut r#type = BitsType::from_static::<usize>();
            let r#type = Gc::new_unchecked(
                NonNull::new_unchecked(&mut r#type as *mut BitsType)
            ).as_nonindexed();

            let v = heap.alloc_nonindexed(r#type);

            assert!(v.is_some());
            let obj = Gc::new_unchecked(v.unwrap().cast::<usize>());
            assert_eq!(obj.r#type(), r#type.as_type());
            assert_eq!(obj.is_marked(), false);
            assert_eq!(*obj.as_ref(), 0usize);
        }
    }
}
