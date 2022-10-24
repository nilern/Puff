use std::collections::hash_map::{self, HashMap};
use std::collections::hash_set::HashSet;
use std::slice;
use std::mem;
use std::iter;

use crate::bytecode::{Bytecode, Opcode, decode_prune_mask, prune_mask_len};
use crate::oref::{Gc, ORef};
use crate::array::Array;
use crate::regs::Regs;
use crate::heap_obj::Indexed;
use crate::mutator::Mutator;
use crate::symbol::Symbol;

// TODO: Warn about (or error on?) dead code

#[derive(Debug)]
pub struct IndexedErr<I> {
    err: Err,
    byte_index: I
}

fn byte_path(parents: &[usize], i: usize) -> Vec<usize> { parents.iter().cloned().chain(iter::once(i)).collect() }

#[derive(Debug)]
pub enum Err {
    InvalidOpcode(u8),
    InvalidPruneMask,
    MissingOpcodeArg(Opcode),

    BranchOut(usize),
    MissingTerminator,

    ConstsOverrun(usize),
    RegsOverrun(usize),
    CloversOverrun(usize),
    RegsOverflow,
    RegsUnderflow,

    TypeError,

    CloversArgc(usize),
    SelfClosureReplaced,

    RegCounts(usize, usize)
}


pub fn verify(mt: &Mutator, code: &Bytecode) -> Result<(), IndexedErr<Vec<usize>>> {
    fn verify_in(mt: &Mutator, bp: &[usize], clovers: &[AbstractType], code: &Bytecode)
        -> Result<(), IndexedErr<Vec<usize>>>
    {
        let cfg = CFG::try_from(code)
            .map_err(|err| IndexedErr {err: err.err, byte_index: byte_path(bp, err.byte_index)})?;

        let mut amts = HashMap::new();
        // DAG, so one pass in topological order is sufficient:
        for leader_index in cfg.post_order().iter().rev() {
            let block = cfg.blocks.get(leader_index).unwrap();

            let mut amt = if *leader_index == 0 {
                let mut amt = AbstractMutator::new(cfg.max_regs, bp);
                amt.push(AbstractType::Closure {len: cfg.clovers_len})?; // 'self' closure
                for _ in 1..cfg.arity {
                    amt.push(AbstractType::Any)?; // arg
                }
                amt
            } else {
                let mut oamt: Option<AbstractMutator> = None;
                for pred_leader_index in block.predecessors.iter() {
                    let pred_amt = amts.get(pred_leader_index).unwrap();
                    oamt = Some(match oamt {
                        Some(amt) => amt.join_at(pred_amt, *leader_index)?,
                        None => {
                            let mut amt = pred_amt.clone();
                            amt.pc = *leader_index;
                            amt
                        }
                    })
                }
                oamt.unwrap() // `block.predecessors` can't be empty since `block` is not the entry block but in RPO
            };

            for stmt in block.stmts.iter() {
                match stmt {
                    &Instr::Define {sym_index} =>
                        match AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) {
                            AbstractType::Symbol => {
                                let t = amt.pop()?;
                                amt.push(t /* HACK */)?;
                                amt.pc += 2;
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::GlobalSet {sym_index} =>
                        match AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) {
                            AbstractType::Symbol => {
                                let t = amt.pop()?;
                                amt.push(t /* HACK */)?;
                                amt.pc += 2;
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::Global {sym_index} =>
                        match AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) {
                            AbstractType::Symbol => {
                                amt.push(AbstractType::Any)?;
                                amt.pc += 2;
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::Const {index} => {
                        let c = amt.get_const(&cfg, index)?;
                        amt.push(AbstractType::of(mt, c))?;
                        amt.pc += 2;
                    },

                    &Instr::Local {index} => {
                        let t = *amt.get_reg(index)?;
                        amt.push(t)?;
                        amt.pc += 2;
                    },

                    &Instr::Clover {index} =>
                        match amt.get_reg(0)? {
                            &AbstractType::Closure {len} =>
                                if index < len {
                                    if len != clovers.len() {
                                        return Err(IndexedErr {
                                            err: Err::SelfClosureReplaced,
                                            byte_index: byte_path(bp, amt.pc)
                                        });
                                    }

                                    amt.push(clovers[index])?;
                                    amt.pc += 2;
                                } else {
                                    return Err(IndexedErr {
                                        err: Err::CloversOverrun(index),
                                        byte_index: byte_path(bp, amt.pc)
                                    });
                                },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::PopNNT {n} => {
                        amt.popnnt(n)?;
                        amt.pc += 2;
                    },

                    &Instr::Prune {prunes} => { // TODO: Warn about unnecessarily long prune mask
                        let regs_len = amt.regs.len();
                        let mut mask_len = 0;
                        let mut free_reg = 0;
                        unsafe {
                            let prunes = prunes as *const u8;
                            let prunes = slice::from_raw_parts(prunes,
                                code.instrs().as_ptr().add(code.instrs().len()) as usize - prunes as usize);
                            for (reg, prune) in decode_prune_mask(prunes).enumerate() {
                                if !prune && reg < regs_len {
                                    *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                                    free_reg += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }
                            }
                        }
                        amt.regs.truncate(free_reg);

                        amt.pc += 1 + mask_len;
                    },

                    &Instr::Box => {
                        amt.pop()?;
                        amt.push(AbstractType::Box)?;
                        amt.pc += 1;
                    },

                    &Instr::UninitializedBox => {
                        amt.push(AbstractType::Box)?;
                        amt.pc += 1;
                    },

                    &Instr::BoxSet => {
                        amt.pop()?;
                        match amt.pop()? {
                            AbstractType::Box => {
                                amt.push(AbstractType::Box)?; // FIXME: Abstraction leak wrt. `set!`-conversion
                                amt.pc += 1;
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        }
                    },

                    &Instr::CheckedBoxSet =>
                        match amt.pop()? {
                            AbstractType::Box => {
                                amt.pop()?;
                                match amt.pop()? {
                                    AbstractType::Box => {
                                        amt.push(AbstractType::Box)?; // FIXME: Abstraction leak wrt. `set!`-conversion
                                        amt.pc += 1;
                                    },
                                    _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                                }
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::BoxGet =>
                        match amt.pop()? {
                            AbstractType::Box => {
                                amt.push(AbstractType::Any)?;
                                amt.pc += 1;
                            },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::CheckedBoxGet =>
                        match amt.pop()? {
                            AbstractType::Box =>
                                match amt.pop()? {
                                    AbstractType::Box => {
                                        amt.push(AbstractType::Any)?;
                                        amt.pc += 1;
                                    },
                                    _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                                },
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::CheckUse =>
                        match amt.pop()? {
                            AbstractType::Box => amt.pc += 1,
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        },

                    &Instr::Fn {code_index, len} => {
                        let code = amt.get_const(&cfg, code_index)?;
                        if let Some(code) = code.try_cast::<Bytecode>(mt) {
                            let clovers_len = unsafe { code.as_ref().clovers_len };
                            if len == clovers_len {
                                if amt.regs.len() >= len {
                                    // Recurse into closure:
                                    unsafe {
                                        let clovers = &amt.regs.as_slice()[amt.regs.len() - clovers_len..];
                                        verify_in(mt, &byte_path(bp, amt.pc), clovers, code.as_ref())?;
                                    }

                                    amt.popn(len)?;
                                    amt.push(AbstractType::Closure {len})?;
                                    amt.pc += 3;
                                } else {
                                    return Err(IndexedErr {
                                        err: Err::RegsOverrun(amt.regs.len()),
                                        byte_index: byte_path(bp, amt.pc)
                                    });
                                }
                            } else {
                                return Err(IndexedErr {err: Err::CloversArgc(len), byte_index: byte_path(bp, amt.pc)});
                            }
                        } else {
                            return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                        }
                    },

                    &Instr::Call {argc, prunes} => { // TODO: Warn about unnecessarily long prune mask
                        amt.popn(argc)?; // Callee and args

                        // Save and restore regs:
                        let regs_len = amt.regs.len();
                        let mut mask_len = 0;
                        let mut free_reg = 0;
                        unsafe {
                            let prunes = prunes as *const u8;
                            let prunes = slice::from_raw_parts(prunes,
                                code.instrs().as_ptr().add(code.instrs().len()) as usize - prunes as usize);
                            for (reg, prune) in decode_prune_mask(prunes).enumerate() {
                                if !prune && reg < regs_len {
                                    *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                                    free_reg += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }
                            }
                        }
                        amt.regs.truncate(free_reg);

                        amt.push(AbstractType::Any)?; // Return value

                        amt.pc += 2 + mask_len;
                    },

                    &Instr::Br {..} | &Instr::Brf {..} | &Instr::Ret | &Instr::TailCall {..} => unreachable!()
                }
            }

            match block.transfer {
                Transfer::Goto {dest: _} => (), // Branching handled by the RPO looping

                Transfer::If {conseq: _, alt: _} => { // Branching handled by the RPO looping
                    amt.pop()?;
                },

                Transfer::Ret =>
                    if amt.regs.len() < 2 { // Not even 'self' closure and return value
                        return Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(bp, amt.pc)});
                    },

                Transfer::TailCall {argc} =>
                    if amt.regs.len() < argc {
                        return Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(bp, amt.pc)});
                    }
            }

            amts.insert(leader_index, amt);
        }

        Ok(())
    }

    verify_in(mt, &[], &[], code)
}

#[derive(Clone, Copy)]
enum AbstractType {
    Bytecode,
    Closure {len: usize},
    Box,
    Symbol,
    Any
}

impl AbstractType {
    fn of(mt: &Mutator, v: ORef) -> Self {
        if let Ok(obj) = Gc::<()>::try_from(v) {
            if obj.instance_of::<Bytecode>(mt) {
                return Self::Bytecode;
            } else if obj.instance_of::<Symbol>(mt) {
                return Self::Symbol;
            }
        }

        Self::Any
    }

    fn join(&self, other: &Self) -> Self {
        use AbstractType::*;

        match *self {
            Any => Any,

            Bytecode => match *other {
                Bytecode => Bytecode,
                _ => Any
            },

            Closure {len: llen} => match *other {
                Closure {len: rlen} if rlen == llen => Closure {len: llen},
                _ => Any
            },

            Box => match *other {
                Box => Box,
                _ => Any
            },

            Symbol => match *other {
                Symbol => Symbol,
                _ => Any
            }
        }
    }
}

#[derive(Clone)]
struct AbstractMutator {
    pc: usize,
    regs: Regs<AbstractType>,

    byte_path: Vec<usize>
}

impl AbstractMutator {
    fn new(max_regs: usize, byte_path: &[usize]) -> Self {
        Self {
            pc: 0,
            regs: Regs::with_capacity(max_regs),

            byte_path: byte_path.iter().cloned().collect()
        }
    }

    fn join_at(&self, other: &Self, pc: usize) -> Result<Self, IndexedErr<Vec<usize>>> {
        debug_assert!(self.regs.capacity() == other.regs.capacity());

        if self.regs.len() != other.regs.len() {
            return Err(IndexedErr {
                err: Err::RegCounts(self.regs.len(), other.regs.len()),
                byte_index: byte_path(&self.byte_path, pc)
            });
        }

        debug_assert!(self.byte_path == other.byte_path);

        let mut regs = Regs::with_capacity(self.regs.capacity());
        unsafe {
            regs.extend_unchecked(self.regs.as_slice().iter()
                .zip(other.regs.as_slice().iter())
                .map(|(lt, rt)| lt.join(rt)));
        }

        Ok(Self {pc, regs, byte_path: self.byte_path.clone()})
    }

    fn get_const(&self, cfg: &CFG, index: usize) -> Result<ORef, IndexedErr<Vec<usize>>> {
        if let Some(&c) = unsafe { cfg.consts.as_ref().indexed_field().get(index) } {
            Ok(c)
        } else {
            Err(IndexedErr {err: Err::ConstsOverrun(index), byte_index: byte_path(&self.byte_path, self.pc) })
        }
    }

    fn push(&mut self, v: AbstractType) -> Result<(), IndexedErr<Vec<usize>>> {
        if self.regs.capacity() >= self.regs.len() + 1 {
            unsafe { self.regs.push_unchecked(v); }
            Ok(())
        } else {
            Err(IndexedErr {err: Err::RegsOverflow, byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }

    fn pop(&mut self) -> Result<AbstractType, IndexedErr<Vec<usize>>> {
        if self.regs.len() >= 1 {
            Ok(unsafe { self.regs.pop_unchecked() })
        } else {
            Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }

    fn popn(&mut self, n: usize) -> Result<(), IndexedErr<Vec<usize>>> {
        if self.regs.len() >= n {
            unsafe { self.regs.popn_unchecked(n); }
            Ok(())
        } else {
            Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }

    fn popnnt(&mut self, n: usize) -> Result<(), IndexedErr<Vec<usize>>> {
        if self.regs.len() >= n + 1 {
            unsafe { self.regs.popnnt_unchecked(n); }
            Ok(())
        } else {
            Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }

    fn get_reg(&self, reg: usize) -> Result<&AbstractType, IndexedErr<Vec<usize>>> {
        if reg < self.regs.len() {
            Ok(&self.regs[reg])
        } else {
            Err(IndexedErr {err: Err::RegsOverrun(reg), byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }

    fn get_reg_mut(&mut self, reg: usize) -> Result<&mut AbstractType, IndexedErr<Vec<usize>>> {
        if reg < self.regs.len() {
            Ok(&mut self.regs[reg])
        } else {
            Err(IndexedErr {err: Err::RegsOverrun(reg), byte_index: byte_path(&self.byte_path, self.pc)})
        }
    }
}

struct CFG<'a> {
    arity: usize,
    max_regs: usize,
    clovers_len: usize,
    consts: Gc<Array<ORef>>,
    blocks: HashMap<usize, Block<'a>>
}

struct Block<'a> {
    predecessors: HashSet<usize>,
    stmts: Vec<Instr<'a>>,
    transfer: Transfer
}

#[derive(Clone, Copy)]
enum Transfer {
    Goto {dest: usize},
    If {conseq: usize, alt: usize},
    Ret,
    TailCall {argc: usize}
}

impl<'a> Block<'a> {
    fn successors(&self) -> Successors {
        match self.transfer {
            Transfer::Goto {dest} => Successors::new([dest, 0], 1),
            Transfer::If {conseq, alt} => Successors::new([conseq, alt], 2),
            Transfer::Ret | Transfer::TailCall {..} => Successors::new([0, 0], 0)
        }
    }
}

struct Successors {
    start: usize,
    end: usize,
    items: [usize; 2]
}

impl Successors {
    fn new(items: [usize; 2], len: usize) -> Self { Successors {start: 0, end: len, items} }
}

impl Iterator for Successors {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            let label = self.items[self.start];
            self.start += 1;
            Some(label)
        } else {
            None
        }
    }
}

impl DoubleEndedIterator for Successors {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.start < self.end {
            let label = self.items[self.end - 1];
            self.end -= 1;
            Some(label)
        } else {
            None
        }
    }
}

impl<'a> TryFrom<&'a Bytecode> for CFG<'a> {
    type Error = IndexedErr<usize>;

    fn try_from(code: &'a Bytecode) -> Result<Self, Self::Error> {
        use Instr::*;

        let instrs = code.instrs();

        let mut blocks = HashMap::new();

        if instrs.len() == 0 { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: 0}); }
        
        let mut i = 0;
        let mut prev_was_branch = false;
        let mut branch_targets: HashMap<usize, HashSet<usize>> = HashMap::new();
        let mut block_start = 0;
        let mut predecessors = HashSet::new();
        let mut stmts = Vec::new();
        while i < instrs.len() {
            if let Some(branching_predecessors) = branch_targets.get(&i) {
                if !prev_was_branch {
                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::Goto {dest: i}
                    });
                    predecessors.insert(block_start);
                    block_start = i;
                }

                predecessors.extend(branching_predecessors);
            }

            let (instr, instr_min_len) = Instr::try_decode(&instrs, i)?;

            match instr {
                Br {dist} => {
                    let dest = i + 1 + dist;
                    if dest < instrs.len() {
                        match branch_targets.entry(dest) {
                            hash_map::Entry::Occupied(mut entry) => { entry.get_mut().insert(block_start); },
                            hash_map::Entry::Vacant(entry) => { entry.insert(iter::once(block_start).collect()); }
                        }
                    } else {
                        return Err(IndexedErr{err: Err::BranchOut(dest), byte_index: i});
                    }

                    i += instr_min_len;
                    if i >= instrs.len() { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: i}); }

                    prev_was_branch = true;

                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::Goto {dest}
                    });
                    block_start = i;
                },

                Brf {dist} => {
                    let alt = i + 1 + dist;
                    if alt < instrs.len() {
                        match branch_targets.entry(alt) {
                            hash_map::Entry::Occupied(mut entry) => { entry.get_mut().insert(block_start); },
                            hash_map::Entry::Vacant(entry) => { entry.insert(iter::once(block_start).collect()); }
                        }
                    } else {
                        return Err(IndexedErr{err: Err::BranchOut(alt), byte_index: i});
                    }

                    i += instr_min_len;
                    if i >= instrs.len() { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: i}); }

                    prev_was_branch = true;

                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::If {conseq: i, alt}
                    });
                    predecessors.insert(block_start);
                    block_start = i;
                },

                Ret => {
                    i += instr_min_len;
                    prev_was_branch = true;

                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::Ret
                    });
                    block_start = i;
                },

                TailCall {argc} => {
                    i += instr_min_len;
                    prev_was_branch = true;

                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::TailCall {argc}
                    });
                    block_start = i;
                },

                Call {argc: _, prunes} | Prune {prunes} => {
                    i += instr_min_len;

                    if let Some(prunes_len) = unsafe {
                        prune_mask_len(slice::from_raw_parts(prunes as *const u8, instrs.len() - i))
                    } {
                        i += prunes_len;
                        if i >= instrs.len() { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: i}); }

                        prev_was_branch = false;
                    } else {
                        return Err(IndexedErr {err: Err::InvalidPruneMask, byte_index: i});
                    }

                    stmts.push(instr);
                },

                Define{..} | GlobalSet{..} | Global{..} | PopNNT{..} | Const{..} | Local{..} | Clover{..}
                | Box | UninitializedBox | BoxSet | CheckedBoxSet | BoxGet | CheckedBoxGet | CheckUse | Fn{..} => {
                    i += instr_min_len;
                    if i >= instrs.len() { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: i}); }

                    prev_was_branch = false;

                    stmts.push(instr);
                }

            }
        }

        if blocks.len() == 0 { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: 0}); }

        Ok(CFG {
            arity: code.arity,
            max_regs: code.max_regs,
            clovers_len: code.clovers_len,
            consts: code.consts,
            blocks
        })
    }
}

impl<'a> CFG<'a> {
    fn post_order(&self) -> Vec<usize> {
        fn block_post_order(visited: &mut HashSet<usize>, po: &mut Vec<usize>, cfg: &CFG, leader_index: usize) {
            if !visited.contains(&leader_index) {
                visited.insert(leader_index);

                for succ in cfg.blocks.get(&leader_index).unwrap().successors().rev() {
                    block_post_order(visited, po, cfg, succ);
                }

                po.push(leader_index);
            }
        }

        let mut po = Vec::new();
        let mut visited = HashSet::new();
        block_post_order(&mut visited, &mut po, self, 0);
        po
    }
}

#[derive(Debug)]
enum Instr<'a> {
    Define {sym_index: usize},
    GlobalSet {sym_index: usize},
    Global {sym_index: usize},

    Const {index: usize},
    Local {index: usize},
    Clover {index: usize},

    PopNNT {n: usize},
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
    TailCall {argc: usize},
    Ret
}

impl<'a> Instr<'a> {
    fn try_decode(bytes: &'a [u8], mut i: usize) -> Result<(Self, usize), IndexedErr<usize>> {
        if let Some(byte) = bytes.get(i) {
            match Opcode::try_from(*byte) {
                Ok(op) => {
                    i += 1;

                    match op {
                        Opcode::Define =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::Define {sym_index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::GlobalSet =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::GlobalSet {sym_index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})                            },

                        Opcode::Global =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::Global {sym_index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Const =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::Const {index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Local =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::Local {index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Clover =>
                            match bytes.get(i) {
                                Some(index) => Ok((Instr::Clover {index: *index as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::PopNNT =>
                            match bytes.get(i) {
                                Some(n) => Ok((Instr::PopNNT {n: *n as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Prune =>
                            match bytes.get(i) {
                                Some(prunes) => Ok((Instr::Prune {prunes: prunes}, 1)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Box => Ok((Instr::Box, 1)),
                        Opcode::UninitializedBox => Ok((Instr::UninitializedBox, 1)),
                        Opcode::BoxSet => Ok((Instr::BoxSet, 1)),
                        Opcode::CheckedBoxSet => Ok((Instr::CheckedBoxSet, 1)),
                        Opcode::BoxGet => Ok((Instr::BoxGet, 1)),
                        Opcode::CheckedBoxGet => Ok((Instr::CheckedBoxGet, 1)),
                        Opcode::CheckUse => Ok((Instr::CheckUse, 1)),

                        Opcode::Brf =>
                            match bytes.get(i) {
                                Some(dist) => Ok((Instr::Brf {dist: *dist as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },
                        Opcode::Br =>
                            match bytes.get(i) {
                                Some(dist) => Ok((Instr::Br {dist: *dist as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Fn =>
                            match bytes.get(i) {
                                Some(code_index) => {
                                    i+= 1;

                                    match bytes.get(i) {
                                        Some(len) => Ok((Instr::Fn {
                                            code_index: *code_index as usize,
                                            len: *len as usize
                                        }, 3)),
                                        None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                                    }
                                },
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Call =>
                            match bytes.get(i) {
                                Some(argc) => {
                                    i += 1;

                                    match bytes.get(i) {
                                        Some(prunes) => Ok((Instr::Call {argc: *argc as usize, prunes: prunes}, 2)),
                                        None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                                    }
                                },
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::TailCall =>
                            match bytes.get(i) {
                                Some(argc) => Ok((Instr::TailCall {argc: *argc as usize}, 2)),
                                None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                            },

                        Opcode::Ret => Ok((Instr::Ret, 1))
                    }
                },

                Err(()) => Err(IndexedErr {err: Err::InvalidOpcode(*byte), byte_index: i})
            }
        } else {
            Err(IndexedErr {err: Err::MissingTerminator, byte_index: i})
        }
    }
}
