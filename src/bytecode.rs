use std::fmt;
use std::mem::transmute;
use std::collections::hash_map::HashMap;
use std::slice;
use std::cmp::Ordering;

use crate::oref::{Reify, ORef, Gc};
use crate::heap_obj::Indexed;
use crate::vector::Vector;
use crate::mutator::Mutator;
use crate::handle::{Handle, HandleT, Root, root};
use crate::r#type::IndexedType;
use crate::compiler::cfg;
use crate::symbol::Symbol;
use crate::write::DisplayWithin;

#[derive(Debug)]
pub enum Opcode {
    Define,
    GlobalSet,
    Global,
    // OPTIMIZE: Define, GlobalSet & Global variants that go through Var instead of Symbol; patched in by VM (like JVM?)

    Const,
    Local,
    Clover,

    Pop,
    Prune,

    Box,
    UninitializedBox,
    BoxSet,
    CheckedBoxSet,
    BoxGet,
    CheckedBoxGet,
    CheckUse,

    Brf,
    Br,

    r#Fn,
    Call,
    CheckOneReturnValue,
    IgnoreReturnValues,
    TailCallWithValues,
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
    let start_len = dest.len();

    for chunk in mask.chunks(7) {
        let mut byte = HIGH_BIT;

        for (i, &prune) in chunk.iter().enumerate() {
            byte |= (prune as u8) << (6 - i);
        }

        dest.push(byte);
    }

    while dest.len() > start_len && dest[dest.len() - 1] == HIGH_BIT {
        dest.pop();
    }

    if dest.len() > start_len {
        let last_index = dest.len() - 1;
        dest[last_index] &= !HIGH_BIT;
    } else {
        dest.push(0);
    }
}

pub fn decode_prune_mask(bytes: &[u8]) -> Prunes {
    Prunes {
        is_nonempty: true,
        byte_index: 0,
        bit_index: 0,
        bytes
    }
}

pub fn prune_mask_len(bytes: &[u8]) -> Option<usize> {
    let mut len = 0;

    loop {
        if let Some(byte) = bytes.get(len) {
            len += 1;

            if byte & HIGH_BIT == 0 { break; }
        } else {
            return None;
        }
    }

    Some(len)
}

pub struct Prunes<'a> {
    is_nonempty: bool,
    byte_index: usize,
    bit_index: usize,
    bytes: &'a [u8]
}

impl<'a> Iterator for Prunes<'a> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_nonempty {
            debug_assert!(self.byte_index < self.bytes.len());
            let byte = self.bytes[self.byte_index];
            let prune = (byte >> (6 - self.bit_index)) & 1 == 1;

            if self.bit_index < 6 {
                self.bit_index += 1;
            } else {
                self.is_nonempty = byte & HIGH_BIT != 0;
                self.byte_index += 1;
                self.bit_index = 0;
            }

            Some(prune)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum DecodedInstr<'a> {
    Define {sym_index: usize},
    GlobalSet {sym_index: usize},
    Global {sym_index: usize},

    Const {index: usize},
    Local {index: usize},
    Clover {index: usize},

    Pop,
    Prune {prunes: &'a u8},

    Box,
    UninitializedBox,
    BoxSet,
    CheckedBoxSet,
    BoxGet,
    CheckedBoxGet,
    CheckUse,

    Brf {dist: usize},
    Br {dist: usize},

    Fn {code_index: usize, len: usize},
    Call {argc: usize, prunes: &'a u8},
    CheckOneReturnValue,
    IgnoreReturnValues,
    TailCallWithValues,
    TailCall {argc: usize},
    Ret
}

impl<'a> DecodedInstr<'a> {
    fn try_decode(bytes: &'a [u8], mut i: usize) -> Option<(Self, usize)> {
        if let Some(byte) = bytes.get(i) {
            match Opcode::try_from(*byte) {
                Ok(op) => {
                    i += 1;

                    match op {
                        Opcode::Define =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::Define {sym_index: *index as usize}, 2)),
                                None => None
                            },

                        Opcode::GlobalSet =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::GlobalSet {sym_index: *index as usize}, 2)),
                                None => None                           },

                        Opcode::Global =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::Global {sym_index: *index as usize}, 2)),
                                None => None
                            },

                        Opcode::Const =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::Const {index: *index as usize}, 2)),
                                None => None
                            },

                        Opcode::Local =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::Local {index: *index as usize}, 2)),
                                None => None
                            },

                        Opcode::Clover =>
                            match bytes.get(i) {
                                Some(index) => Some((DecodedInstr::Clover {index: *index as usize}, 2)),
                                None => None
                            },

                        Opcode::Pop => Some((DecodedInstr::Pop, 1)),

                        Opcode::Prune =>
                            match bytes.get(i) {
                                Some(prunes) => Some((DecodedInstr::Prune {prunes: prunes}, 1)),
                                None => None
                            },

                        Opcode::Box => Some((DecodedInstr::Box, 1)),
                        Opcode::UninitializedBox => Some((DecodedInstr::UninitializedBox, 1)),
                        Opcode::BoxSet => Some((DecodedInstr::BoxSet, 1)),
                        Opcode::CheckedBoxSet => Some((DecodedInstr::CheckedBoxSet, 1)),
                        Opcode::BoxGet => Some((DecodedInstr::BoxGet, 1)),
                        Opcode::CheckedBoxGet => Some((DecodedInstr::CheckedBoxGet, 1)),
                        Opcode::CheckUse => Some((DecodedInstr::CheckUse, 1)),

                        Opcode::Brf =>
                            match bytes.get(i) {
                                Some(dist) => Some((DecodedInstr::Brf {dist: *dist as usize}, 2)),
                                None => None
                            },
                        Opcode::Br =>
                            match bytes.get(i) {
                                Some(dist) => Some((DecodedInstr::Br {dist: *dist as usize}, 2)),
                                None => None
                            },

                        Opcode::Fn =>
                            match bytes.get(i) {
                                Some(code_index) => {
                                    i+= 1;

                                    match bytes.get(i) {
                                        Some(len) => Some((DecodedInstr::Fn {
                                            code_index: *code_index as usize,
                                            len: *len as usize
                                        }, 3)),
                                        None => None
                                    }
                                },
                                None => None
                            },

                        Opcode::Call =>
                            match bytes.get(i) {
                                Some(argc) => {
                                    i += 1;

                                    match bytes.get(i) {
                                        Some(prunes) => Some((DecodedInstr::Call {argc: *argc as usize, prunes: prunes}, 2)),
                                        None => None
                                    }
                                },
                                None => None
                            },

                        Opcode::CheckOneReturnValue => Some((DecodedInstr::CheckOneReturnValue, 1)),

                        Opcode::IgnoreReturnValues => Some((DecodedInstr::IgnoreReturnValues, 1)),

                        Opcode::TailCallWithValues => Some((DecodedInstr::TailCallWithValues, 1)),

                        Opcode::TailCall =>
                            match bytes.get(i) {
                                Some(argc) => Some((DecodedInstr::TailCall {argc: *argc as usize}, 2)),
                                None => None
                            },

                        Opcode::Ret => Some((DecodedInstr::Ret, 1))
                    }
                },

                Err(()) => None
            }
        } else {
            None
        }
    }
}

#[repr(C)]
pub struct Bytecode {
    pub min_arity: usize,
    pub varargs: bool,
    pub max_regs: usize,
    pub clovers_len: usize,
    pub clover_names: Gc<Vector<ORef>>,
    pub consts: Gc<Vector<ORef>>,
    pub positions: Gc<Vector<ORef>> // OPTIMIZE: Compress with e.g. run-length encoding or even DWARF style
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
    pub fn new(mt: &mut Mutator, min_arity: usize, varargs: bool, max_regs: usize, clovers_len: usize,
        clover_names: HandleT<Vector<ORef>>, consts: HandleT<Vector<ORef>>, positions: HandleT<Vector<ORef>>,
        instrs: &[u8]
    ) -> Gc<Self> {
        unsafe {
            let r#type = Self::reify(mt).unchecked_cast::<IndexedType>();
            let mut nptr = mt.alloc_indexed(r#type, instrs.len()).cast::<Self>();

            nptr.as_ptr().write(Bytecode {
                min_arity,
                varargs,
                max_regs,
                clovers_len,
                clover_names: *clover_names,
                consts: *consts,
                positions: *positions
            });
            nptr.as_mut().indexed_field_mut().copy_from_slice(instrs);

            Gc::new_unchecked(nptr)
        }
    }

    pub fn instrs(&self) -> &[u8] { self.indexed_field() }

    fn disassemble(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        unsafe {
            write!(fmt, "{}(clovers {}) ", indent, self.clovers_len)?;
            if self.min_arity > 0 {
                write!(fmt, "(_")?;

                for _ in 1..self.min_arity {
                    write!(fmt, " _")?;
                }

                if self.varargs {
                    write!(fmt, " . _")?;
                }

                writeln!(fmt, ")")?;
            } else {
                if !self.varargs {
                    writeln!(fmt, "()")?;
                } else {
                    writeln!(fmt, "_")?;
                }
            }
            writeln!(fmt, "{}(locals {})", indent, self.max_regs)?;

            let mut instrs = self.instrs().iter().enumerate();
            while let Some((i, &byte)) = instrs.next() {
                if let Ok(op) = Opcode::try_from(byte) {
                    match op {
                        Opcode::Define =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self.consts.as_ref().indexed_field()[*ci as usize];
                                writeln!(fmt, "{}{}: define {} ; {}", indent, i, ci, c.within(mt))?;
                            } else {
                                todo!()
                            },

                        Opcode::GlobalSet =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self.consts.as_ref().indexed_field()[*ci as usize];
                                writeln!(fmt, "{}{}: global-set! {} ; {}", indent, i, ci, c.within(mt))?;
                            } else {
                                todo!()
                            },

                        Opcode::Global =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self.consts.as_ref().indexed_field()[*ci as usize];
                                writeln!(fmt, "{}{}: global {} ; {}", indent, i, ci, c.within(mt))?;
                            } else {
                                todo!()
                            },

                        Opcode::Const =>
                            if let Some((_, ci)) = instrs.next() {
                                let c = self.consts.as_ref().indexed_field()[*ci as usize];
                                writeln!(fmt, "{}{}: const {} ; {}", indent, i, ci, c.within(mt))?;
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
                                let name = self.clover_names.as_ref().indexed_field()[*j as usize];

                                write!(fmt, "{}{}: clover {}", indent, i, j)?;
                                if let Some(name) = name.try_cast::<Symbol>(mt) {
                                    writeln!(fmt, "; {}", name.as_ref().name())?;
                                } else {
                                    writeln!(fmt, "")?;
                                }
                            } else {
                                todo!()
                            },

                        Opcode::Pop => writeln!(fmt, "{}{}: pop", indent, i)?,

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

                        Opcode::Box => writeln!(fmt, "{}{}: box", indent, i)?,
                        Opcode::UninitializedBox => writeln!(fmt, "{}{}: uninitialized-box", indent, i)?,
                        Opcode::BoxSet => writeln!(fmt, "{}{}: box-set!", indent, i)?,
                        Opcode::CheckedBoxSet => writeln!(fmt, "{}{}: checked-box-set!", indent, i)?,
                        Opcode::BoxGet => writeln!(fmt, "{}{}: box-get", indent, i)?,
                        Opcode::CheckedBoxGet => writeln!(fmt, "{}{}: checked-box-get", indent, i)?,
                        Opcode::CheckUse => writeln!(fmt, "{}{}: check-use", indent, i)?,

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
                            if let Some((_, ci)) = instrs.next() {
                                if let Some((_, len)) = instrs.next() {
                                    let code = self.consts.as_ref().indexed_field()[*ci as usize];

                                    if let Some(code) = code.try_cast::<Bytecode>(mt) {
                                        writeln!(fmt, "{}{}: fn {}", indent, i, len)?;
                                        code.as_ref().disassemble(mt, fmt, &(indent.to_string() + "  "))?;
                                    } else {
                                        todo!()
                                    }
                                } else {
                                    todo!()
                                }
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

                        Opcode::CheckOneReturnValue => writeln!(fmt, "{}{}: check-one-return-value", indent, i)?,

                        Opcode::IgnoreReturnValues => writeln!(fmt, "{}{}: ignore-return-values", indent, i)?,

                        Opcode::TailCallWithValues => writeln!(fmt, "{}{}: tailcall-with-values", indent, i)?,

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

    pub fn pc_pos(&self, target_pc: usize) -> Option<ORef> {
        let instrs = self.instrs();

        let mut pc = 0;
        let mut i = 0;
        while pc < instrs.len() {
            match pc.cmp(&target_pc) {
                Ordering::Less =>
                    match DecodedInstr::try_decode(instrs, pc) {
                        Some((instr, instr_min_len)) => {
                            pc += instr_min_len;

                            match instr {
                                DecodedInstr::Prune {prunes} | DecodedInstr::Call {argc: _, prunes} =>
                                    pc += unsafe {
                                        prune_mask_len(slice::from_raw_parts(prunes as *const u8, instrs.len() - pc))
                                    }.unwrap(),

                                _ => ()
                            }

                            i += 1;
                        },

                        None => return None
                    },

                Ordering::Equal => return Some(unsafe { self.positions.as_ref().indexed_field()[i] }),

                Ordering::Greater => return None
            }
        }

        None
    }
}

pub struct Builder {
    min_arity: usize,
    varargs: bool,
    max_regs: usize,
    clovers_len: usize,
    clover_names: HandleT<Vector<ORef>>,
    consts: Vec<Handle>,
    instrs: Vec<u8>,
    label_indices: HashMap<cfg::Label, usize>,
    br_dests: HashMap<usize, cfg::Label>,
    positions: Vec<Handle>
}

impl Builder {
    pub fn new(min_arity: usize, varargs: bool, max_regs: usize, clover_names: HandleT<Vector<ORef>>) -> Self {
        Self {
            min_arity,
            varargs,
            max_regs,
            clovers_len: unsafe { clover_names.as_ref().indexed_field().len() },
            clover_names,
            consts: Vec::new(),
            instrs: Vec::new(),
            label_indices: HashMap::new(),
            br_dests: HashMap::new(),
            positions: Vec::new()
        }
    }

    // TODO: Deduplicate constants
    pub fn define(&mut self, name: HandleT<Symbol>, pos: Handle) {
        self.instrs.push(Opcode::Define as u8);

        let i = u8::try_from(self.consts.len()).unwrap();
        self.consts.push(name.into());
        self.instrs.push(i);
        self.positions.push(pos);
    }

    // TODO: Deduplicate constants
    pub fn global_set(&mut self, name: HandleT<Symbol>, pos: Handle) {
        self.instrs.push(Opcode::GlobalSet as u8);

        let i = u8::try_from(self.consts.len()).unwrap();
        self.consts.push(name.into());
        self.instrs.push(i);
        self.positions.push(pos);
    }

    // TODO: Deduplicate constants
    pub fn global(&mut self, name: HandleT<Symbol>, pos: Handle) {
        self.instrs.push(Opcode::Global as u8);

        let i = u8::try_from(self.consts.len()).unwrap();
        self.consts.push(name.into());
        self.instrs.push(i);
        self.positions.push(pos);
    }

    // TODO: Deduplicate constants
    pub fn r#const(&mut self, v: Handle, pos: Handle) {
        self.instrs.push(Opcode::Const as u8);

        let i = u8::try_from(self.consts.len()).unwrap();
        self.consts.push(v);
        self.instrs.push(i);
        self.positions.push(pos);
    }

    pub fn local(&mut self, reg: usize, pos: Handle) {
        self.instrs.push(Opcode::Local as u8);
        self.instrs.push(u8::try_from(reg).unwrap());
        self.positions.push(pos);
    }

    pub fn clover(&mut self, i: usize, pos: Handle) {
        self.instrs.push(Opcode::Clover as u8);
        self.instrs.push(u8::try_from(i).unwrap());
        self.positions.push(pos);
    }

    pub fn pop(&mut self, pos: Handle) {
        self.instrs.push(Opcode::Pop as u8);
        self.positions.push(pos);
    }

    pub fn prune(&mut self, prunes: &[bool], pos: Handle) {
        self.instrs.push(Opcode::Prune as u8);
        encode_prune_mask(&mut self.instrs, prunes);
        self.positions.push(pos);
    }

    pub fn r#box(&mut self, pos: Handle) {
        self.instrs.push(Opcode::Box as u8);
        self.positions.push(pos);
    }

    pub fn uninitialized_box(&mut self, pos: Handle) {
        self.instrs.push(Opcode::UninitializedBox as u8);
        self.positions.push(pos);
    }

    pub fn box_set(&mut self, pos: Handle) {
        self.instrs.push(Opcode::BoxSet as u8);
        self.positions.push(pos);
    }

    pub fn checked_box_set(&mut self, pos: Handle) {
        self.instrs.push(Opcode::CheckedBoxSet as u8);
        self.positions.push(pos);
    }

    pub fn box_get(&mut self, pos: Handle) {
        self.instrs.push(Opcode::BoxGet as u8);
        self.positions.push(pos);
    }

    pub fn checked_box_get(&mut self, pos: Handle) {
        self.instrs.push(Opcode::CheckedBoxGet as u8);
        self.positions.push(pos);
    }

    pub fn check_use(&mut self, pos: Handle) {
        self.instrs.push(Opcode::CheckUse as u8);
        self.positions.push(pos);
    }

    pub fn label(&mut self, label: cfg::Label) {
        self.label_indices.insert(label, self.instrs.len());
    }

    pub fn brf(&mut self, label: cfg::Label, pos: Handle) {
        self.instrs.push(Opcode::Brf as u8);
        self.br_dests.insert(self.instrs.len(), label);
        self.instrs.push(0);
        self.positions.push(pos);
    }

    pub fn br(&mut self, label: cfg::Label, pos: Handle) {
        self.instrs.push(Opcode::Br as u8);
        self.br_dests.insert(self.instrs.len(), label);
        self.instrs.push(0);
        self.positions.push(pos);
    }

    pub fn r#fn(&mut self, code: HandleT<Bytecode>, len: usize, pos: Handle) {
        self.instrs.push(Opcode::r#Fn as u8);

        let i = u8::try_from(self.consts.len()).unwrap();
        self.consts.push(code.into());
        self.instrs.push(i);

        self.instrs.push(u8::try_from(len).unwrap());
        self.positions.push(pos);
    }

    pub fn call(&mut self, cargc: usize, prune_mask: &[bool], pos: Handle) {
        self.instrs.push(Opcode::Call as u8);
        self.instrs.push(u8::try_from(cargc).unwrap());
        encode_prune_mask(&mut self.instrs, prune_mask);
        self.positions.push(pos);
    }

    pub fn check_one_return_value(&mut self, pos: Handle) {
        self.instrs.push(Opcode::CheckOneReturnValue as u8);
        self.positions.push(pos);
    }

    pub fn ignore_return_values(&mut self, pos: Handle) {
        self.instrs.push(Opcode::IgnoreReturnValues as u8);
        self.positions.push(pos);
    }

    pub fn tailcall_with_values(&mut self, pos: Handle) {
        self.instrs.push(Opcode::TailCallWithValues as u8);
        self.positions.push(pos);
    }

    pub fn tailcall(&mut self, cargc: usize, pos: Handle) {
        self.instrs.push(Opcode::TailCall as u8);
        self.instrs.push(u8::try_from(cargc).unwrap());
        self.positions.push(pos);
    }

    pub fn ret(&mut self, pos: Handle) {
        self.instrs.push(Opcode::Ret as u8);
        self.positions.push(pos);
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

        let consts = root!(mt, Vector::<ORef>::from_handles(mt, &self.consts));
        let positions = root!(mt, Vector::<ORef>::from_handles(mt, &self.positions));
        Bytecode::new(mt, self.min_arity, self.varargs, self.max_regs, self.clovers_len, self.clover_names, consts,
            positions, &self.instrs)
    }
}
