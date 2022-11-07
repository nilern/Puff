use std::alloc::{Layout, alloc, dealloc};
use std::cell::Cell;
use std::mem::{size_of, align_of, transmute};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::marker::PhantomData;

use crate::oref::{ORef, Gc, Reify};
use crate::heap_obj::HeapObj;
use crate::mutator::Mutator;
use crate::heap::{self, Heap};
use crate::r#type::Type;

pub struct HandleRef<'a, T>(&'a Gc<T>);

impl<'a, T> Clone for HandleRef<'a, T> {
    fn clone(&self) -> Self { Self(self.0) }
}

impl<'a, T> Copy for HandleRef<'a, T> {}

impl<'a, T> Deref for HandleRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &'a Self::Target { unsafe { self.0.as_ref() } }
}

impl<'a, T> HandleRef<'a, T> {
    pub fn oref(self) -> Gc<T> { *self.0 }
}

pub struct HandleRefAny<'a>(&'a ORef);

impl<'a> Clone for HandleRefAny<'a> {
    fn clone(&self) -> Self { Self(self.0) }
}

impl<'a> Copy for HandleRefAny<'a> {}

impl<'a, T> From<HandleRef<'a, T>> for HandleRefAny<'a> {
    fn from(href: HandleRef<'a, T>) -> Self { Self(unsafe { transmute::<&'a Gc<T>, &'a ORef>(href.0) }) }
}

impl<'a> HandleRefAny<'a> {
    pub fn oref(self) -> ORef { *self.0 }
}

struct LiveHandleImpl {
    oref: ORef,
    rc: Cell<usize>,
    next: Option<NonNull<LiveHandleImpl>>
}

struct FreeHandleImpl {
    next: Option<NonNull<FreeHandleImpl>>
}

pub struct HandleAny(*mut LiveHandleImpl);

impl Clone for HandleAny {
    fn clone(&self) -> Self {
        unsafe { (*self.0).rc.set((*self.0).rc.get() + 1); }

        HandleAny(self.0)
    }
}

impl Drop for HandleAny {
    fn drop(&mut self) {
        unsafe { (*self.0).rc.set((*self.0).rc.get() - 1); }
    }
}

impl HandleAny {
    pub fn oref(&self) -> ORef { unsafe { (*self.0).oref } }
}

impl HandleAny {
    pub fn borrow<'a>(&self) -> HandleRefAny<'a> { HandleRefAny(unsafe { &(*self.0).oref }) }

    pub fn try_cast<U: Reify>(self, mt: &Mutator) -> Option<Handle<U>> where Gc<U::Kind>: Into<Gc<Type>> {
        match Handle::<()>::try_from(self) {
            Ok(obj_handle) => obj_handle.try_cast::<U>(mt),
            Err(()) => None
        }
    }
}

pub struct Handle<T> {
    handle: HandleAny,
    phantom: PhantomData<*const T>
}

impl<T> Deref for Handle<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { transmute::<_, &Self::Target>((*self.handle.0).oref.unchecked_cast::<T>().as_ptr()) }
    }
}

impl<T> DerefMut for Handle<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { transmute::<_, &mut Self::Target>((*self.handle.0).oref.unchecked_cast::<T>().as_ptr()) }
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self { Self { handle: self.handle.clone(), phantom: self.phantom } }
}

impl TryFrom<HandleAny> for Handle<()> {
    type Error = ();

    fn try_from(handle: HandleAny) -> Result<Self, Self::Error> {
        if handle.oref().tag() == Gc::<()>::TAG {
            Ok(Self {handle, phantom: Default::default()})
        } else {
            Err(())
        }
    }
}

impl<T> From<Handle<T>> for HandleAny {
    fn from(typed: Handle<T>) -> Self { typed.handle }
}

impl<T> Handle<T> {
    pub fn oref(&self) -> Gc<T> { unsafe { self.handle.oref().unchecked_cast::<T>() } }

    pub fn borrow<'a>(&'a self) -> HandleRef<'a, T> {
        HandleRef(unsafe { transmute::<&ORef, &Gc<T>>(&(*self.handle.0).oref) })
    }

    unsafe fn unchecked_cast<U>(self) -> Handle<U> { Handle {handle: self.handle, phantom: PhantomData::default()} }
}

impl<T: HeapObj> Handle<T> {
    pub fn try_cast<U: Reify>(self, mt: &Mutator) -> Option<Handle<U>> where Gc<U::Kind>: Into<Gc<Type>> {
        if self.handle.oref().instance_of::<U>(mt) {
            Some(unsafe { self.unchecked_cast::<U>() })
        } else {
            None
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
    pub unsafe fn root(&mut self, oref: ORef) -> HandleAny {
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

        HandleAny(handle.as_ptr())
    }

    pub unsafe fn root_t<T>(&mut self, obj: Gc<T>) -> Handle<T> {
        Handle {
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

    pub fn for_each_mut_root_freeing<F: FnMut(&mut ORef)>(&mut self, mut f: F) {
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

    pub unsafe fn verify(&self, heap: &Heap) -> Result<(), heap::VerificationError> {
        let mut curr = self.live;

        while let Some(curr_ptr) = curr {
            let curr_ref = curr_ptr.as_ref();

            if curr_ref.rc.get() > 0 {
                heap.verify_oref(curr_ref.oref)?;
            }

            curr = curr_ref.next;
        }

        Ok(())
    }
}

pub trait Root {
    type Rooted;

    fn root(self, mt: &mut Mutator) -> Self::Rooted;
}

impl Root for ORef {
    type Rooted = HandleAny;

    fn root(self, mt: &mut Mutator) -> Self::Rooted { mt.root(self) }
}

impl<T> Root for Gc<T> {
    type Rooted = Handle<T>;

    fn root(self, mt: &mut Mutator) -> Self::Rooted { mt.root_t(self) }
}

macro_rules! root {
    ($mt:expr, $e:expr) => {{
        let oref = $e;
        oref.root($mt)
    }}
}

pub(crate) use root;

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::fixnum::Fixnum;

    #[test]
    fn new_pool() {
        let handles = HandlePool::new();

        assert!(handles.free.is_none());
        assert!(handles.live.is_none());
        assert_eq!(handles.chunks.len(), 0);
    }

    #[test]
    fn for_each_mut_root_freeing() {
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
            handles.for_each_mut_root_freeing(|&mut oref| roots.push(oref));
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

            assert_eq!(handle.oref(), ORef::from(Fixnum::try_from(5isize).unwrap()));
        }
    }
}
