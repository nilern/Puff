use std::os::unix::io::RawFd;
use std::str;
use std::cell::Cell;
use libc::c_int;
use nix::unistd;
use nix::sys::stat;
use nix::fcntl;

use crate::mutator::Mutator;
use crate::oref::{Reify, Gc};
use crate::handle::HandleRef;
use crate::fixnum::Fixnum;
use crate::heap_obj::{NonIndexed, Singleton};
use crate::r#type::NonIndexedType;
use crate::string::String;
use crate::vector::VectorMut;

#[repr(C)]
pub struct Eof;

impl Reify for Eof {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().eof }
}

impl Singleton for Eof {
    fn instance(mt: &Mutator) -> Gc<Self> { mt.singletons().eof }
}

#[repr(C)]
pub struct Port {
    fd: RawFd,
    buf: Gc<VectorMut<u8>>,
    buf_start: Cell<usize>,
    buf_len: Cell<usize>
}

impl Reify for Port {
    type Kind = NonIndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().port }
}

unsafe impl NonIndexed for Port {}

impl Port {
    const BUFFER_SIZE: usize = 4096;

    pub fn open(mt: &mut Mutator, filename: HandleRef<String>, oflag: Fixnum) -> Gc<Self> {
        let fd = fcntl::open(
            filename.as_str(),
            unsafe { fcntl::OFlag::from_bits_unchecked(isize::from(oflag) as c_int) },
            stat::Mode::empty()
        ).unwrap();
        let buf = VectorMut::<u8>::zeros(mt, Self::BUFFER_SIZE);

        unsafe {
            let nptr = mt.alloc_static::<Self>();
            nptr.as_ptr().write(Port {fd, buf, buf_start: Cell::new(0), buf_len: Cell::new(0)});
            Gc::new_unchecked(nptr)
        }
    }

    fn fill_buf(&self) {
        self.buf_start.set(0);
        self.buf_len.set(unistd::read(
            self.fd as c_int,
            unsafe { self.buf.as_ref().as_bytes_mut() }
        ).unwrap());
    }

    // OPTIMIZE:
    pub fn read_char(&self) -> Option<char> {
        if self.buf_len.get() == 0 {
            self.fill_buf();
        }

        if self.buf_len.get() == 0 {
            return None;
        }

        let bytes = unsafe { self.buf.as_ref().as_bytes() };
        let start = self.buf_start.get();
        let s = match str::from_utf8(&bytes[start..(start + 4)]) {
            Ok(s) => s,
            Err(err) => match err.valid_up_to() {
                0 => todo!("pending input or invalid UTF-8"),
                len => unsafe { str::from_utf8_unchecked(&bytes[0..len]) } // SAFETY: err.valid_up_to()
            }
        };

        let mut cis = s.char_indices();
        let (_, c) = cis.next().unwrap();
        let c_len = match cis.next() {
            Some((len, _)) => len,
            None => s.len()
        };

        self.buf_start.set(self.buf_start.get() + c_len);
        self.buf_len.set(self.buf_len.get() - c_len);
        Some(c)
    }
}
