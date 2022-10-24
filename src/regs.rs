use std::mem::{size_of, align_of, transmute};
use std::alloc::{Layout, alloc, dealloc};
use std::slice;
use std::ops::{Index, IndexMut};

use crate::mutator::Mutator;
use crate::oref::ORef;

pub struct Regs<T: Copy> {
    start: *mut T,
    end: *mut T,
    base: *mut T,
    top: *mut T
}

impl<T: Copy> Clone for Regs<T> {
    fn clone(&self) -> Self {
        let mut other = Self::with_capacity(self.capacity());
        unsafe {
            other.base.copy_from_nonoverlapping(self.base, self.len());
            other.top = other.base.add(self.len());
        }
        other
    }
}

impl<T: Copy> Drop for Regs<T> {
    fn drop(&mut self) {
        unsafe {
            dealloc(self.start as *mut u8, Layout::from_size_align_unchecked(
                self.end as usize - self.start as usize,
                Self::ALIGN
            ));
        }
    }
}

impl<T: Copy> Index<usize> for Regs<T> {
    type Output = T;

    fn index<'a>(&'a self, index: usize) -> &'a Self::Output {
        unsafe { transmute::<*mut T, &'a T>(self.base.add(index)) }
    }
}

impl<T: Copy> IndexMut<usize> for Regs<T> {
    fn index_mut<'a>(&'a mut self, index: usize) -> &'a mut Self::Output {
        unsafe { transmute::<*mut T, &'a mut T>(self.base.add(index)) }
    }
}

impl<T: Copy> Regs<T> {
    const ALIGN: usize = align_of::<T>();

    pub fn with_capacity(cap: usize) -> Self {
        unsafe {
            let start = alloc(Layout::from_size_align_unchecked(cap * size_of::<T>(), Self::ALIGN)) as *mut T;
            Self {
                start,
                end: start.add(cap),
                base: start,
                top: start
            }
        }
    }

    pub fn capacity(&self) -> usize { (self.end as usize - self.start as usize) / size_of::<T>() }

    pub fn len(&self) -> usize { (self.top as usize - self.base as usize) / size_of::<T>() }

    pub fn ensure(&mut self, required: usize) {
        let required_bytes = required * size_of::<T>();

        if (self.end as usize - self.base as usize) < required_bytes {
            let len = self.len();

            if (self.end as usize - self.start as usize) >= required_bytes {
                unsafe {
                    self.base.copy_to(self.start, len);

                    self.base = self.start;
                    self.top = self.base.add(len);
                }
            } else {
                unsafe {
                    let start = alloc(Layout::from_size_align_unchecked(required_bytes, Self::ALIGN))
                        as *mut T;
                    self.base.copy_to_nonoverlapping(start, len);

                    self.start = start;
                    self.end = start.add(required);
                    self.base = start;
                    self.top = start.add(len);
                }
            }
        }
    }

    fn reserve(&mut self, additional: usize) { self.ensure(self.len() + additional); }

    pub fn as_slice(&self) -> &[T] { unsafe { slice::from_raw_parts(self.base, self.len()) } }

    pub unsafe fn pop_unchecked(&mut self) -> T {
        self.top = self.top.sub(1);
        debug_assert!(self.top >= self.base);
        *self.top
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.base < self.top {
            Some(unsafe { self.pop_unchecked() })
        } else {
            None
        }
    }

    pub unsafe fn popn_unchecked(&mut self, n: usize) {
        self.top = self.top.sub(n);
        debug_assert!(self.top >= self.base);
    }

    pub unsafe fn popnnt_unchecked(&mut self, n: usize) {
        let top = *self.top.sub(1);
        self.top = self.top.sub(n);
        debug_assert!(self.top >= self.base);
        *self.top.sub(1) = top;
    }

    pub unsafe fn push_unchecked(&mut self, v: T) {
        debug_assert!(self.top < self.end);
        self.top.write(v);
        self.top = self.top.add(1);
    }

    pub fn push(&mut self, v: T) {
        self.reserve(1);
        unsafe { self.push_unchecked(v); }
    }

    pub unsafe fn extend_unchecked<I: Iterator<Item=T>>(&mut self, vs: I) {
        let mut top = self.top;
        for v in vs {
            top.write(v);
            top = top.add(1);
        }
        self.top = top;
    }

    pub fn enter(&mut self, new_len: usize) {
        unsafe { self.base = self.top.sub(new_len); }
    }

    pub fn re_enter(&mut self, saveds: &[T]) {
        let required = saveds.len() + 1;

        unsafe {
            let base = self.top.sub(required);
            if base >= self.start {
                base.copy_from_nonoverlapping(saveds.as_ptr(), saveds.len());

                self.base = base;
            } else {
                let required_bytes = required * size_of::<T>();

                if (self.end as usize - self.start as usize) < required_bytes {
                    self.start.copy_from_nonoverlapping(saveds.as_ptr(), saveds.len());
                    self.start.add(saveds.len()).write(*self.top);

                    self.base = self.start;
                    self.top = self.start.add(required);
                } else {
                    let start = alloc(Layout::from_size_align_unchecked(required_bytes, Self::ALIGN))
                        as *mut T;

                    start.copy_from_nonoverlapping(saveds.as_ptr(), saveds.len());
                    start.add(saveds.len()).write(*self.top);

                    self.start = start;
                    self.end = start.add(required);
                    self.base = start;
                    self.top = start.add(required);
                }
            }
        }
    }

    pub fn truncate(&mut self, n: usize) {
        if n < self.len() {
            unsafe { self.top = self.base.add(n); }
        }
    }
}

impl Regs<ORef> {
    pub fn dump(&self, mt: &Mutator) {
        print!("[");
        for v in self.as_slice() { print!("{}, ", v.within(mt)); }
        println!("]");
    }
}
