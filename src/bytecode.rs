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
    Local,
    Clover,
    PopNNT,
    Brf,
    Br,
    r#Fn,
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
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result {
        writeln!(fmt, "#<bytecode")?;
        unsafe { self.as_ref().disassemble(mt, fmt, "  ")?; }
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

    fn disassemble(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        unsafe {
            let mut instrs = self.instrs().iter().enumerate();
            while let Some((i, &byte)) = instrs.next() {
                if let Ok(op) = Opcode::try_from(byte) {
                    match op {
                        Opcode::Const =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self
                                    .consts.as_ref()
                                    .indexed_field()[*ci as usize];
                                if let Some(code) = c.try_cast::<Bytecode>(mt) {
                                    writeln!(fmt, "{}{}: const {}", indent, i, ci)?;
                                    code.as_ref().disassemble(mt, fmt, &(indent.to_string() + "  "))?;
                                } else {
                                    writeln!(fmt, "{}{}: const {} ; {}", indent, i, ci,
                                        c.within(mt))?;
                                }
                            } else {
                                todo!()
                            },

                        Opcode::Local =>
                            if let Some((_, reg)) = instrs.next() {
                                writeln!(fmt, "{}{}: local {}", indent, i, reg)?;
                            } else {
                                todo!()
                            },

                        Opcode::Clover =>
                            if let Some((_, j)) = instrs.next() {
                                writeln!(fmt, "{}{}: clover {}", indent, i, j)?;
                            } else {
                                todo!()
                            },

                        Opcode::PopNNT =>
                            if let Some((_, n)) = instrs.next() {
                                writeln!(fmt, "{}{}: popnnt {}", indent, i, n)?;
                            } else {
                                todo!()
                            },

                        Opcode::Brf =>
                            if let Some((_, d)) = instrs.next() {
                                writeln!(fmt, "{}{}: brf {}", indent, i, d)?;
                            } else {
                                todo!()
                            },

                        Opcode::Br =>
                            if let Some((_, d)) = instrs.next() {
                                writeln!(fmt, "{}{}: br {}", indent, i, d)?;
                            } else {
                                todo!()
                            },

                        Opcode::r#Fn =>
                            if let Some((_, len)) = instrs.next() {
                                writeln!(fmt, "{}{}: fn {}", indent, i, len)?;
                            } else {
                                todo!()
                            },

                        Opcode::Ret => writeln!(fmt, "{}{}: ret", indent, i)?
                    }
                } else {
                    todo!();
                }
            }
        }

        Ok(())
    }
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

    pub fn local(&mut self, reg: usize) {
        self.instrs.push(Opcode::Local as u8);
        self.instrs.push(u8::try_from(reg).unwrap());
    }

    pub fn clover(&mut self, i: usize) {
        self.instrs.push(Opcode::Clover as u8);
        self.instrs.push(u8::try_from(i).unwrap());
    }

    pub fn popnnt(&mut self, n: usize) {
        self.instrs.push(Opcode::PopNNT as u8);
        self.instrs.push(u8::try_from(n).unwrap());
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

    pub fn r#fn(&mut self, len: usize) {
        self.instrs.push(Opcode::r#Fn as u8);
        self.instrs.push(u8::try_from(len).unwrap());
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
