use std::fmt;
use std::mem::transmute;
use std::collections::hash_map::HashMap;

use crate::oref::{Reify, DisplayWithin, ORef, Gc};
use crate::heap_obj::Indexed;
use crate::array::Array;
use crate::mutator::Mutator;
use crate::handle::{Handle, HandleT};
use crate::r#type::IndexedType;

#[derive(Debug)]
pub enum Opcode {
    Const,
    Local,
    Clover,
    PopNNT,
    Prune,
    Brf,
    Br,
    r#Fn,
    Call,
    TailCall,
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

const HIGH_BIT: u8 = 0b10000000;

fn encode_prune_mask(dest: &mut Vec<u8>, mask: &[bool]) {
    for chunk in mask.chunks(7) {
        let mut byte = HIGH_BIT;

        for (i, &prune) in chunk.iter().enumerate() {
            byte |= (prune as u8) << (6 - i);
        }

        dest.push(byte);
    }

    while dest.len() > 0 && dest[dest.len() - 1] == HIGH_BIT {
        dest.pop();
    }

    if dest.len() > 0 {
        let last_index = dest.len() - 1;
        dest[last_index] &= !HIGH_BIT;
    } else {
        dest.push(0);
    }
}

pub fn decode_prune_mask(bytes: &[u8]) -> Prunes {
    let byte = *bytes.get(0).unwrap();
    Prunes {
        is_nonempty: true,
        byte_index: 0,
        bit_index: 0,
        byte,
        bytes
    }
}

pub struct Prunes<'a> {
    is_nonempty: bool,
    byte_index: usize,
    bit_index: usize,
    byte: u8,
    bytes: &'a [u8]
}

impl<'a> Iterator for Prunes<'a> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_nonempty {
            let prune = (self.byte >> (6 - self.bit_index)) & 1 == 1;

            if self.bit_index < 6 {
                self.bit_index += 1;
            } else {
                self.is_nonempty = self.byte & HIGH_BIT == HIGH_BIT;

                if self.is_nonempty {
                    self.byte_index += 1;
                    self.bit_index = 0;
                    self.byte = self.bytes[self.byte_index];
                }
            }

            Some(prune)
        } else {
            None
        }
    }
}

#[repr(C)]
pub struct Bytecode {
    pub arity: usize,
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
    pub const TYPE_LEN: usize = 3;

    pub fn new(mt: &mut Mutator, arity: usize, consts: HandleT<Array<ORef>>, instrs: &[u8])
        -> Gc<Self>
    {
        unsafe {
            let r#type = Self::reify(mt).unchecked_cast::<IndexedType>();
            if let Some (nptr) = mt.alloc_indexed(r#type, instrs.len()) {
                let mut nptr = nptr.cast::<Self>();

                nptr.as_ptr().write(Bytecode {
                    arity,
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
            write!(fmt, "{}(", indent)?;
            if self.arity > 0 {
                write!(fmt, "_")?;

                for _ in 1..self.arity {
                    write!(fmt, " _")?;
                }
            }
            writeln!(fmt, ")")?;

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

                        Opcode::Prune => {
                            write!(fmt, "{}{}: prune #b", indent, i)?;

                            let mut mask_len = 0;
                            for (i, prune) in decode_prune_mask(&self.instrs()[i + 1..]).enumerate() {
                                write!(fmt, "{}", prune as u8)?;
                                if i % 7 == 0 {
                                    mask_len += 1;
                                }
                            }

                            writeln!(fmt, "")?;

                            for _ in 0..mask_len {
                                instrs.next();
                            }
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

                        Opcode::Call =>
                            if let Some((j, argc)) = instrs.next() {
                                write!(fmt, "{}{}: call {} #b", indent, i, argc)?;

                                let mut mask_len = 0;
                                for (i, prune) in decode_prune_mask(&self.instrs()[j + 1..]).enumerate() {
                                    write!(fmt, "{}", (prune as u8))?;
                                    if i % 7 == 0 {
                                        mask_len += 1;
                                    }
                                }

                                writeln!(fmt, "")?;

                                for _ in 0..mask_len {
                                    instrs.next();
                                }
                            } else {
                                todo!()
                            },

                        Opcode::TailCall =>
                            if let Some((_, argc)) = instrs.next() {
                                writeln!(fmt, "{}{}: tailcall {}", indent, i, argc)?;
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
    arity: usize,
    consts: Vec<Handle>,
    instrs: Vec<u8>,
    label_indices: HashMap<usize, usize>,
    br_dests: HashMap<usize, usize>
}

impl Builder {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            consts: Vec::new(),
            instrs: Vec::new(),
            label_indices: HashMap::new(),
            br_dests: HashMap::new()
        }
    }

    // TODO: Deduplicate constants
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

    // Optimize: No-op if n = 0:
    pub fn popnnt(&mut self, n: usize) {
        self.instrs.push(Opcode::PopNNT as u8);
        self.instrs.push(u8::try_from(n).unwrap());
    }

    // OPITIMIZE: No-op if nothing to prune:
    pub fn prune(&mut self, prunes: &[bool]) {
        self.instrs.push(Opcode::Prune as u8);
        encode_prune_mask(&mut self.instrs, prunes);
    }

    pub fn label(&mut self, label: usize) {
        self.label_indices.insert(label, self.instrs.len());
    }

    pub fn brf(&mut self, label: usize) {
        self.instrs.push(Opcode::Brf as u8);
        self.br_dests.insert(self.instrs.len(), label);
        self.instrs.push(0);
    }

    pub fn br(&mut self, label: usize) {
        self.instrs.push(Opcode::Br as u8);
        self.br_dests.insert(self.instrs.len(), label);
        self.instrs.push(0);
    }

    pub fn r#fn(&mut self, len: usize) {
        self.instrs.push(Opcode::r#Fn as u8);
        self.instrs.push(u8::try_from(len).unwrap());
    }

    pub fn call(&mut self, argc: usize, prune_mask: &[bool]) {
        self.instrs.push(Opcode::Call as u8);
        self.instrs.push(u8::try_from(argc.checked_add(1).unwrap()).unwrap());
        encode_prune_mask(&mut self.instrs, prune_mask);
    }

    pub fn tailcall(&mut self, argc: usize) {
        self.instrs.push(Opcode::TailCall as u8);
        self.instrs.push(u8::try_from(argc.checked_add(1).unwrap()).unwrap());
    }

    pub fn ret(&mut self) {
        self.instrs.push(Opcode::Ret as u8);
    }

    fn backpatch(&mut self) {
        for (&i, dest) in self.br_dests.iter() {
            if let Ok(d) = u8::try_from(self.label_indices[dest] - i) {
                self.instrs[i] = d;
            } else {
                todo!()
            }
        }
    }

    pub fn build(mut self, mt: &mut Mutator) -> Gc<Bytecode> {
        self.backpatch();

        let consts = Array::<ORef>::from_handles(mt, &self.consts);
        let consts = mt.root_t(consts);
        Bytecode::new(mt, self.arity, consts, &self.instrs)
    }
}
