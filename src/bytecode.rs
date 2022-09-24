use std::fmt;
use std::mem::transmute;

use crate::oref::{Reify, DisplayWithin, ORef, Gc};
use crate::heap_obj::Indexed;
use crate::array::Array;
use crate::mutator::Mutator;
use crate::handle::{Handle, HandleT};
use crate::r#type::IndexedType;

pub enum Opcode {
    Const,
    Brf,
    Br,
    Ret
}

impl TryFrom<u8> for Opcode {
    type Error = ();

    fn try_from(byte: u8) -> Result<Self, Self::Error> {
        if byte <= Opcode::Ret as u8 {
            Ok(unsafe { transmute(byte) })
        } else {
            Err(())
        }
    }
}

#[repr(C)]
pub struct Bytecode {
    pub consts: Gc<Array<ORef>>
}

unsafe impl Indexed for Bytecode {
    type Item = u8;
}

impl Reify for Bytecode {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().bytecode }
}

impl DisplayWithin for Gc<Bytecode> {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result
    {
        writeln!(fmt, "#<bytecode")?;

        unsafe {
            let mut instrs = self.as_ref().instrs().iter().enumerate();
            while let Some((i, &byte)) = instrs.next() {
                if let Ok(op) = Opcode::try_from(byte) {
                    match op {
                        Opcode::Const =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self.as_ref()
                                    .consts.as_ref()
                                    .indexed_field()[*ci as usize];
                                writeln!(fmt, "  {}: const {} ; {}", i, ci,
                                    c.within(mt))?;
                            } else {
                                todo!()
                            },

                        Opcode::Brf =>
                            if let Some((_, d)) = instrs.next() {
                                writeln!(fmt, "  {}: brf {}", i, d)?;
                            } else {
                                todo!()
                            },

                        Opcode::Br =>
                            if let Some((_, d)) = instrs.next() {
                                writeln!(fmt, "  {}: br {}", i, d)?;
                            } else {
                                todo!()
                            },

                        Opcode::Ret => writeln!(fmt, "  {}: ret", i)?
                    }
                } else {
                    todo!();
                }
            }
        }

        write!(fmt, ">")
    }
}

impl Bytecode {
    pub const TYPE_LEN: usize = 2;

    pub fn new(mt: &mut Mutator, consts: HandleT<Array<ORef>>, instrs: &[u8])
        -> Gc<Self>
    {
        unsafe {
            let r#type = Self::reify(mt).unchecked_cast::<IndexedType>();
            if let Some (nptr) = mt.alloc_indexed(r#type, instrs.len()) {
                let mut nptr = nptr.cast::<Self>();

                nptr.as_ptr().write(Bytecode {
                    consts: *consts
                });
                nptr.as_mut().indexed_field_mut().copy_from_slice(instrs);

                Gc::new_unchecked(nptr)
            } else {
                todo!() // Need to GC, then retry
            }
        }
    }

    pub fn instrs(&self) -> &[u8] { self.indexed_field() }
}

pub struct Builder {
    consts: Vec<Handle>,
    instrs: Vec<u8>
}

impl Builder {
    pub fn new() -> Self {
        Self {
            consts: Vec::new(),
            instrs: Vec::new()
        }
    }

    pub fn r#const(&mut self, mt: &mut Mutator, v: ORef) {
        self.instrs.push(Opcode::Const as u8);

        if let Ok(i) = u8::try_from(self.consts.len()) {
            self.consts.push(mt.root(v));
            self.instrs.push(i);
        } else {
            todo!()
        }
    }

    #[must_use]
    pub fn brf(&mut self) -> usize {
        self.instrs.push(Opcode::Brf as u8);
        let i = self.instrs.len();
        self.instrs.push(0);
        i
    }

    #[must_use]
    pub fn br(&mut self) -> usize {
        self.instrs.push(Opcode::Br as u8);
        let i = self.instrs.len();
        self.instrs.push(0);
        i
    }

    pub fn backpatch(&mut self, i: usize) {
        if let Ok(d) = u8::try_from(self.instrs.len() - i) {
            self.instrs[i] = d;
        } else {
            todo!()
        }
    }

    pub fn ret(&mut self) {
        self.instrs.push(Opcode::Ret as u8);
    }

    pub fn build(self, mt: &mut Mutator) -> Gc<Bytecode> {
        let consts = Array::<ORef>::from_handles(mt, &self.consts);
        let consts = mt.root_t(consts);
        Bytecode::new(mt, consts, &self.instrs)
    }
}
