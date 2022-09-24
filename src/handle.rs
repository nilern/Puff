use std::alloc::{Layout, alloc, dealloc};
use std::cell::Cell;
use std::mem::{size_of, align_of, transmute};
use std::ops::Deref;
use std::ptr::NonNull;
use std::marker::PhantomData;

use crate::oref::{ORef, Gc};

struct LiveHandleImpl {
    oref: ORef,
    rc: Cell<usize>,
    next: Option<NonNull<LiveHandleImpl>>
}

struct FreeHandleImpl {
    next: Option<NonNull<FreeHandleImpl>>
}

pub struct Handle(*const LiveHandleImpl);

impl Deref for Handle {
    type Target = ORef;

    fn deref(&self) -> &Self::Target { unsafe { &(*self.0).oref } }
}

impl Clone for Handle {
    fn clone(&self) -> Self {
        unsafe { (*self.0).rc.set((*self.0).rc.get() + 1); }

        Handle(self.0)
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        unsafe { (*self.0).rc.set((*self.0).rc.get() - 1); }
    }
}

pub struct HandleT<T> {
    handle: Handle,
    phantom: PhantomData<*const T>
}

impl<T> Deref for HandleT<T> {
    type Target = Gc<T>;

    fn deref(&self) -> &Self::Target {
        unsafe { transmute::<_, &Self::Target>(&*self.handle) }
    }
}

impl TryFrom<Handle> for HandleT<()> {
    type Error = ();

    fn try_from(handle: Handle) -> Result<Self, Self::Error> {
        if handle.tag() == Gc::<()>::TAG {
            Ok(Self {handle, phantom: Default::default()})
        } else {
            Err(())
        }
    }
}
 
pub struct HandlePool {
    free: Option<NonNull<FreeHandleImpl>>,
    live: Option<NonNull<LiveHandleImpl>>,
    chunks: Vec<*mut u8>
}

impl Drop for HandlePool {
    fn drop(&mut self) {
        for &chunk in self.chunks.iter() {
            unsafe { dealloc(chunk, Self::CHUNK_LAYOUT); }
        }
    }
}

impl HandlePool {
    const CHUNK_SIZE: usize = 1 << 12; // 4k, a common page size

    const CHUNK_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(
        Self::CHUNK_SIZE,
        align_of::<LiveHandleImpl>()
    ) };

    pub fn new() -> Self {
        HandlePool {
            free: None,
            live: None,
            chunks: Vec::new()
        }
    }

    /// Safety: The returned handle and its clones are only valid for the
    /// lifetime of `self`.
    pub unsafe fn root(&mut self, oref: ORef) -> Handle {
        if self.free.is_none() {
            self.grow();
        }

        let free = self.free.unwrap();
        let live = self.live;

        // Pop `self.free`:
        self.free = free.as_ref().next;
        let handle = free.as_ptr() as *mut LiveHandleImpl;

        // Initialize `handle`:
        handle.write(LiveHandleImpl{
            oref,
            rc: Cell::new(1),
            next: live
        });

        // Push to `self.live`:
        let handle = NonNull::new_unchecked(handle);
        self.live = Some(handle);

        Handle(handle.as_ptr())
    }

    pub unsafe fn root_t<T>(&mut self, obj: Gc<T>) -> HandleT<T> {
        HandleT {
            handle: self.root(obj.into()),
            phantom: Default::default()
        }
    }

    fn grow(&mut self) {
        let len = Self::CHUNK_SIZE / size_of::<LiveHandleImpl>();

        // Allocate:
        let chunk = unsafe { alloc(Self::CHUNK_LAYOUT) };
        self.chunks.push(chunk);

        // Link into `self.free`:
        unsafe {
            let mut handle = (chunk as *mut LiveHandleImpl).add(len);
            let mut free = self.free;
            for _ in 0..len {
                handle = handle.offset(-1);
                let new_free = handle as *mut FreeHandleImpl;

                new_free.write(FreeHandleImpl {next: free});
                free = Some(NonNull::new_unchecked(new_free));
            }

            self.free = free;
        }
    }

    fn for_each_root<F: FnMut(&mut ORef)>(&mut self, mut f: F) {
        let mut prev: Option<NonNull<LiveHandleImpl>> = None;
        let mut curr = self.live;

        while let Some(mut curr_ptr) = curr {
            let curr_ref = unsafe { curr_ptr.as_mut() };

            if curr_ref.rc.get() == 0 {
                unsafe { self.free(prev, curr_ref); }
            } else {
                f(&mut curr_ref.oref);

                prev = curr;
            }

            curr = curr_ref.next;
        }
    }

    unsafe fn free(&mut self, prev: Option<NonNull<LiveHandleImpl>>,
        curr: &mut LiveHandleImpl
    ) {
        // Unlink from `self.live`:
        if let Some(mut prev) = prev {
            prev.as_mut().next = curr.next;
        } else {
            self.live = curr.next;
        }

        // Link into `self.free`:
        let free = (curr as *mut LiveHandleImpl) as *mut FreeHandleImpl;
        free.write(FreeHandleImpl {next: self.free});
        self.free = Some(NonNull::new_unchecked(free));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::oref::Fixnum;

    #[test]
    fn new_pool() {
        let handles = HandlePool::new();

        assert!(handles.free.is_none());
        assert!(handles.live.is_none());
        assert_eq!(handles.chunks.len(), 0);
    }

    #[test]
    fn for_each_root() {
        let mut handles = HandlePool::new();

        unsafe {
            handles.root(Fixnum::try_from(0isize).unwrap().into());
            let _handle1 =
                handles.root(Fixnum::try_from(1isize).unwrap().into());
            handles.root(Fixnum::try_from(2isize).unwrap().into());
            handles.root(Fixnum::try_from(3isize).unwrap().into());
            handles.root(Fixnum::try_from(4isize).unwrap().into());
            let _handle5 =
                handles.root(Fixnum::try_from(5isize).unwrap().into());
            handles.root(Fixnum::try_from(6isize).unwrap().into());

            // Only called on live roots:
            let mut roots = Vec::<ORef>::new();
            handles.for_each_root(|&mut oref| roots.push(oref));
            assert_eq!(roots, vec![
                Fixnum::try_from(5isize).unwrap().into(),
                Fixnum::try_from(1isize).unwrap().into()
            ]);

            // Only live roots remain internally:
            let mut n = 0;
            let mut curr = handles.live;
            while let Some(mut curr_ptr) = curr {
                n += 1;
                curr = curr_ptr.as_mut().next;
            }
            assert_eq!(n, roots.len());
        }
    }

    #[test]
    fn handle_lifespan() {
        let mut handles = HandlePool::new();

        unsafe {
            let handle1 =
                handles.root(Fixnum::try_from(5isize).unwrap().into());

            let handle_impl = handle1.0;

            assert_eq!(
                (*handle_impl).oref,
                ORef::from(Fixnum::try_from(5isize).unwrap())
            );
            assert_eq!((*handle_impl).rc.get(), 1);
            assert_eq!((*handle_impl).next, None);


            {
                let handle2 = handle1.clone();

                let handle1_impl = handle1.0;
                assert_eq!(
                    (*handle1_impl).oref,
                    ORef::from(Fixnum::try_from(5isize).unwrap())
                );
                assert_eq!((*handle1_impl).rc.get(), 2);
                assert_eq!((*handle1_impl).next, None);

                assert_eq!(handle2.0, handle1_impl);
            }

            let handle_impl = handle1.0;
            assert_eq!(
                (*handle_impl).oref,
                ORef::from(Fixnum::try_from(5isize).unwrap())
            );
            assert_eq!((*handle_impl).rc.get(), 1);
            assert_eq!((*handle_impl).next, None);
        }
    }

    #[test]
    fn five_handles() {
        let mut handles = HandlePool::new();

        unsafe {
            let handle1 =
                handles.root(Fixnum::try_from(5isize).unwrap().into());
            let handle2 =
                handles.root(Fixnum::try_from(23isize).unwrap().into());

            let handle1_impl = handle1.0;
            let handle2_impl = handle2.0;

            assert!(handle1_impl != handle2_impl);

            assert_eq!(
                (*handle1_impl).oref,
                ORef::from(Fixnum::try_from(5isize).unwrap())
            );
            assert_eq!((*handle1_impl).rc.get(), 1);
            assert_eq!((*handle1_impl).next, None);

            assert_eq!(
                (*handle2_impl).oref,
                ORef::from(Fixnum::try_from(23isize).unwrap())
            );
            assert_eq!((*handle2_impl).rc.get(), 1);
            assert_eq!(
                (*handle2_impl).next,
                Some(NonNull::new_unchecked(handle1.0 as *mut LiveHandleImpl))
            );
        }

    }

    #[test]
    fn handle_deref() {
        let mut handles = HandlePool::new();

        unsafe {
            let handle = handles.root(Fixnum::try_from(5isize).unwrap().into());

            assert_eq!(*handle, ORef::from(Fixnum::try_from(5isize).unwrap()));
        }
    }
}
