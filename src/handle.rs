use std::alloc::{Layout, alloc, dealloc};
use std::cell::{Cell, RefCell};
use std::mem::{size_of, align_of};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;

use crate::oref::ORef;

struct LiveHandleImpl {
    oref: ORef,
    rc: Cell<usize>,
    next: Option<NonNull<LiveHandleImpl>>
}

struct FreeHandleImpl {
    next: Option<NonNull<FreeHandleImpl>>
}

struct Handle<'a>(&'a LiveHandleImpl);

impl<'a> Deref for Handle<'a> {
    type Target = ORef;

    fn deref(&self) -> &'a Self::Target { &self.0.oref }
}

impl<'a> Clone for Handle<'a> {
    fn clone(&self) -> Self {
        self.0.rc.set(self.0.rc.get() + 1);

        Handle(self.0)
    }
}

impl<'a> Drop for Handle<'a> {
    fn drop(&mut self) {
        self.0.rc.set(self.0.rc.get() - 1);
    }
}
 
struct HandlePool<'a> {
    free: Cell<Option<NonNull<FreeHandleImpl>>>,
    live: Cell<Option<NonNull<LiveHandleImpl>>>,
    chunks: RefCell<Vec<*mut u8>>,
    phantom: PhantomData<&'a LiveHandleImpl>
}

impl<'a> Drop for HandlePool<'a> {
    fn drop(&mut self) {
        for &chunk in self.chunks.borrow().iter() {
            unsafe { dealloc(chunk, Self::CHUNK_LAYOUT); }
        }
    }
}

impl<'a> HandlePool<'a> {
    const CHUNK_SIZE: usize = 1 << 12; // 4k, a common page size

    const CHUNK_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(
        Self::CHUNK_SIZE,
        align_of::<LiveHandleImpl>()
    ) };

    fn new() -> Self {
        HandlePool {
            free: Cell::new(None),
            live: Cell::new(None),
            chunks: RefCell::new(Vec::new()),
            phantom: PhantomData::default()
        }
    }

    fn root(&'a self, oref: ORef) -> Handle<'a> {
        if self.free.get().is_none() {
            self.grow();
        }

        unsafe {
            let free = self.free.get().unwrap();
            let live = self.live.get();

            // Pop `self.free`:
            self.free.set(free.as_ref().next);
            let handle = free.as_ptr() as *mut LiveHandleImpl;

            // Initialize `handle`:
            handle.write(LiveHandleImpl{
                oref,
                rc: Cell::new(1),
                next: live
            });

            // Push to `self.live`:
            let handle = NonNull::new_unchecked(handle);
            self.live.set(Some(handle));

            Handle(handle.as_ref())
        }
    }

    fn grow(&'a self) {
        let len = Self::CHUNK_SIZE / size_of::<LiveHandleImpl>();

        // Allocate:
        let chunk = unsafe { alloc(Self::CHUNK_LAYOUT) };
        self.chunks.borrow_mut().push(chunk);

        // Link into `self.free`:
        unsafe {
            let mut handle = (chunk as *mut LiveHandleImpl).add(len);
            let mut free = self.free.get();
            for _ in 0..len {
                handle = handle.offset(-1);
                let new_free = handle as *mut FreeHandleImpl;

                new_free.write(FreeHandleImpl {next: free});
                free = Some(NonNull::new_unchecked(new_free));
            }

            self.free.set(free);
        }
    }

    fn for_each_root<F: FnMut(&mut ORef)>(&'a self, mut f: F) {
        let mut prev: Option<NonNull<LiveHandleImpl>> = None;
        let mut curr = self.live.get();

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

    unsafe fn free(&'a self, prev: Option<NonNull<LiveHandleImpl>>,
        curr: &mut LiveHandleImpl
    ) {
        // Unlink from `self.live`:
        if let Some(mut prev) = prev {
            prev.as_mut().next = curr.next;
        } else {
            self.live.set(curr.next);
        }

        // Link into `self.free`:
        let free = (curr as *mut LiveHandleImpl) as *mut FreeHandleImpl;
        free.write(FreeHandleImpl {next: self.free.get()});
        self.free.set(Some(NonNull::new_unchecked(free)));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::oref::Fixnum;

    #[test]
    fn new_pool() {
        let handles = HandlePool::new();

        assert!(handles.free.get().is_none());
        assert!(handles.live.get().is_none());
    }

    #[test]
    fn for_each_root() {
        let handles = HandlePool::new();

        handles.root(Fixnum::try_from(0isize).unwrap().into());
        let _handle1 = handles.root(Fixnum::try_from(1isize).unwrap().into());
        handles.root(Fixnum::try_from(2isize).unwrap().into());
        handles.root(Fixnum::try_from(3isize).unwrap().into());
        handles.root(Fixnum::try_from(4isize).unwrap().into());
        let _handle5 = handles.root(Fixnum::try_from(5isize).unwrap().into());
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
        let mut curr = handles.live.get();
        while let Some(mut curr_ptr) = curr {
            n += 1;
            curr = unsafe { curr_ptr.as_mut() }.next;
        }
        assert_eq!(n, roots.len());
    }

    #[test]
    fn handle_lifespan() {
        let handles = HandlePool::new();

        let handle1 = handles.root(Fixnum::try_from(5isize).unwrap().into());

        let handle_impl = handle1.0;

        assert_eq!(
            handle_impl.oref,
            ORef::from(Fixnum::try_from(5isize).unwrap())
        );
        assert_eq!(handle_impl.rc.get(), 1);
        assert_eq!(handle_impl.next, None);


        {
            let handle2 = handle1.clone();

            let handle1_impl = handle1.0;
            assert_eq!(
                handle1_impl.oref,
                ORef::from(Fixnum::try_from(5isize).unwrap())
            );
            assert_eq!(handle1_impl.rc.get(), 2);
            assert_eq!(handle1_impl.next, None);

            assert_eq!(
                handle2.0 as *const LiveHandleImpl,
                handle1_impl as *const LiveHandleImpl);
        }

        let handle_impl = handle1.0;
        assert_eq!(
            handle_impl.oref,
            ORef::from(Fixnum::try_from(5isize).unwrap())
        );
        assert_eq!(handle_impl.rc.get(), 1);
        assert_eq!(handle_impl.next, None);
    }

    #[test]
    fn five_handles() {
        let handles = HandlePool::new();

        let handle1 = handles.root(Fixnum::try_from(5isize).unwrap().into());
        let handle2 = handles.root(Fixnum::try_from(23isize).unwrap().into());

        unsafe {
            let handle1_impl = handle1.0;
            let handle2_impl = handle2.0;

            assert!(
                handle1_impl as *const LiveHandleImpl
                != handle2_impl as *const LiveHandleImpl
            );

            assert_eq!(
                handle1_impl.oref,
                ORef::from(Fixnum::try_from(5isize).unwrap())
            );
            assert_eq!(handle1_impl.rc.get(), 1);
            assert_eq!(handle1_impl.next, None);

            assert_eq!(
                handle2_impl.oref,
                ORef::from(Fixnum::try_from(23isize).unwrap())
            );
            assert_eq!(handle2_impl.rc.get(), 1);
            assert_eq!(
                handle2_impl.next,
                Some(NonNull::new_unchecked(
                    (handle1.0 as *const LiveHandleImpl) as *mut LiveHandleImpl
                ))
            );
        }

    }

    #[test]
    fn handle_deref() {
        let handles = HandlePool::new();

        let handle = handles.root(Fixnum::try_from(5isize).unwrap().into());

        assert_eq!(*handle, ORef::from(Fixnum::try_from(5isize).unwrap()));
    }
}
