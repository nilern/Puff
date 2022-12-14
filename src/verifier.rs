use std::collections::hash_map::{self, HashMap};
use std::collections::hash_set::HashSet;
use std::slice;
use std::mem;
use std::iter;

use crate::bytecode::{Bytecode, Opcode, DecodedInstr, decode_prune_mask, prune_mask_len};
use crate::oref::{Gc, ORef};
use crate::vector::Vector;
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

    UnexpectedInstr,

    BranchOut(usize),
    MissingTerminator,
    MissingReturnValuesHandler,

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
        enum Continue {
            Stmt,
            Transfer,
            Block
        }

        fn verify_stmt<'a, I>(mt: &Mutator, code: &'a Bytecode, bp: &[usize], clovers: &[AbstractType],
            cfg: &'a CFG, amt: &mut AbstractMutator, stmts: &mut I, transfer: &Transfer
        ) -> Result<Continue, IndexedErr<Vec<usize>>> where I: Iterator<Item=&'a DecodedInstr<'a>>
        {
            let stmt = match stmts.next() {
                Some(stmt) => stmt,
                None => return Ok(Continue::Transfer)
            };

            match stmt {
                &DecodedInstr::Define {sym_index} => {
                    if AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) != AbstractType::Symbol {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    let t = amt.pop()?;
                    amt.push(t /* HACK */)?;
                    amt.pc += 2;
                },

                &DecodedInstr::GlobalSet {sym_index} => {
                    if AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) != AbstractType::Symbol {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    let t = amt.pop()?;
                    amt.push(t /* HACK */)?;
                    amt.pc += 2;
                },

                &DecodedInstr::Global {sym_index} => {
                    if AbstractType::of(mt, amt.get_const(&cfg, sym_index)?) != AbstractType::Symbol {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.push(AbstractType::Any)?;
                    amt.pc += 2;
                },

                &DecodedInstr::Const {index} => {
                    let c = amt.get_const(&cfg, index)?;
                    amt.push(AbstractType::of(mt, c))?;
                    amt.pc += 2;
                },

                &DecodedInstr::Local {index} => {
                    let t = *amt.get_reg(index)?;
                    amt.push(t)?;
                    amt.pc += 2;
                },

                &DecodedInstr::Clover {index} =>
                    if let &AbstractType::Closure {len} = amt.get_reg(0)? {
                        if index >= len {
                            return Err(IndexedErr {
                                err: Err::CloversOverrun(index),
                                byte_index: byte_path(bp, amt.pc)
                            });
                        }

                        if len != clovers.len() {
                            return Err(IndexedErr {
                                err: Err::SelfClosureReplaced,
                                byte_index: byte_path(bp, amt.pc)
                            });
                        }

                        amt.push(clovers[index])?;
                        amt.pc += 2;
                    } else {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                    },

                &DecodedInstr::Pop => {
                    amt.pop()?;
                    amt.pc += 1;
                },

                &DecodedInstr::Prune {prunes} => { // TODO: Warn about unnecessarily long prune mask
                    let regs_len = amt.regs.len();
                    let mut reg = 0;
                    let mut mask_len = 0;
                    let mut free_reg = 0;
                    unsafe {
                        let prunes = prunes as *const u8;
                        let prunes = slice::from_raw_parts(prunes,
                            code.instrs().as_ptr().add(code.instrs().len()) as usize - prunes as usize);
                        for prune in decode_prune_mask(prunes) {
                            if !prune && reg < regs_len {
                                *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                                free_reg += 1;
                            }

                            if reg % 7 == 0 {
                                mask_len += 1;
                            }

                            reg += 1;
                        }

                        for reg in reg..regs_len {
                            *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                            free_reg += 1;
                        }
                    }
                    amt.regs.truncate(free_reg);

                    amt.pc += 1 + mask_len;
                },

                &DecodedInstr::Box => {
                    amt.pop()?;
                    amt.push(AbstractType::Box)?;
                    amt.pc += 1;
                },

                &DecodedInstr::UninitializedBox => {
                    amt.push(AbstractType::Box)?;
                    amt.pc += 1;
                },

                &DecodedInstr::BoxSet => {
                    amt.pop()?;
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.push(AbstractType::Box)?; // FIXME: Abstraction leak wrt. `set!`-conversion
                    amt.pc += 1;
                },

                &DecodedInstr::CheckedBoxSet => {
                    amt.pop()?;
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.push(AbstractType::Box)?; // FIXME: Abstraction leak wrt. `set!`-conversion
                    amt.pc += 1;
                },

                &DecodedInstr::BoxGet => {
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.push(AbstractType::Any)?;
                    amt.pc += 1;
                },

                &DecodedInstr::CheckedBoxGet => {
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.push(AbstractType::Any)?;
                    amt.pc += 1;
                },

                &DecodedInstr::CheckUse => {
                    if amt.pop()? != AbstractType::Box {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }

                    amt.pc += 1;
                },

                &DecodedInstr::Fn {code_index, len} => {
                    let code = amt.get_const(&cfg, code_index)?;
                    if let Some(code) = code.try_cast::<Bytecode>(mt) {
                        let clovers_len = mt.borrow(code).clovers_len;
                        if len != clovers_len {
                            return Err(IndexedErr {err: Err::CloversArgc(len), byte_index: byte_path(bp, amt.pc)});
                        }

                        if amt.regs.len() < len {
                            return Err(IndexedErr {
                                err: Err::RegsOverrun(amt.regs.len()),
                                byte_index: byte_path(bp, amt.pc)
                            });
                        }

                        // Recurse into closure:
                        let clovers = &amt.regs.as_slice()[amt.regs.len() - clovers_len..];
                        verify_in(mt, &byte_path(bp, amt.pc), clovers, mt.borrow(code))?;

                        amt.popn(len)?;
                        amt.push(AbstractType::Closure {len})?;
                        amt.pc += 3;
                    } else {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }
                },

                &DecodedInstr::DomainFn {arity, code_index, len} => {
                    let code = amt.get_const(&cfg, code_index)?;
                    if let Some(code) = code.try_cast::<Bytecode>(mt) {
                        let clovers_len = mt.borrow(code).clovers_len;
                        if len != clovers_len {
                            return Err(IndexedErr {err: Err::CloversArgc(len), byte_index: byte_path(bp, amt.pc)});
                        }

                        if amt.regs.len() < len {
                            return Err(IndexedErr {
                                err: Err::RegsOverrun(amt.regs.len()),
                                byte_index: byte_path(bp, amt.pc)
                            });
                        }

                        // Recurse into closure:
                        let clovers = &amt.regs.as_slice()[amt.regs.len() - clovers_len..];
                        verify_in(mt, &byte_path(bp, amt.pc), clovers, mt.borrow(code))?;

                        amt.popn(arity + len)?;
                        amt.push(AbstractType::Closure {len})?;
                        amt.pc += 3;
                    } else {
                        return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)});
                    }
                },

                &DecodedInstr::CaseFn {clausec} => {
                    for _ in 0..clausec {
                        match amt.pop()? {
                            AbstractType::Closure {..} => (),
                            _ => return Err(IndexedErr {err: Err::TypeError, byte_index: byte_path(bp, amt.pc)})
                        }
                    }

                    amt.push(AbstractType::CaseFn)?;
                    amt.pc += 2;
                },

                &DecodedInstr::Call {argc, prunes} => { // TODO: Warn about unnecessarily long prune mask
                    amt.popn(argc)?; // Callee and args

                    // Save and restore regs:
                    let regs_len = amt.regs.len();
                    let mut reg = 0;
                    let mut mask_len = 0;
                    let mut free_reg = 0;
                    unsafe {
                        let prunes = prunes as *const u8;
                        let prunes = slice::from_raw_parts(prunes,
                            code.instrs().as_ptr().add(code.instrs().len()) as usize - prunes as usize);
                        for prune in decode_prune_mask(prunes) {
                            if !prune && reg < regs_len {
                                *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                                free_reg += 1;
                            }

                            if reg % 7 == 0 {
                                mask_len += 1;
                            }

                            reg += 1;
                        }

                        for reg in reg..regs_len {
                            *amt.get_reg_mut(free_reg)? = *amt.get_reg(reg)?;
                            free_reg += 1;
                        }
                    }
                    amt.regs.truncate(free_reg);

                    amt.pc += 2 + mask_len;

                    match stmts.next() {
                        Some(stmt) =>
                            match stmt {
                                &DecodedInstr::CheckOneReturnValue => {
                                    amt.push(AbstractType::Any)?; // Return value
                                    amt.pc += 1;
                                },

                                &DecodedInstr::IgnoreReturnValues => amt.pc += 1,

                                _ => return Err(IndexedErr {
                                    err: Err::UnexpectedInstr,
                                    byte_index: byte_path(bp, amt.pc)
                                })
                            },

                        None =>
                            match transfer {
                                &Transfer::TailCallWithValues =>
                                    if amt.regs.len() < 2 { // Not even 'self' closure and consumer fn
                                        return Err(IndexedErr {
                                            err: Err::RegsUnderflow,
                                            byte_index: byte_path(bp, amt.pc)
                                        });
                                    } else {
                                        return Ok(Continue::Block);
                                    },

                                _ => return Err(IndexedErr {
                                    err: Err::MissingReturnValuesHandler,
                                    byte_index: byte_path(bp, amt.pc)
                                })
                            }
                    }
                },

                &DecodedInstr::CheckOneReturnValue | &DecodedInstr::IgnoreReturnValues
                | &DecodedInstr::TailCallWithValues =>
                    return Err(IndexedErr {err: Err::UnexpectedInstr, byte_index: byte_path(bp, amt.pc)}),

                &DecodedInstr::Br {..} | &DecodedInstr::Brf {..} | &DecodedInstr::Ret | &DecodedInstr::TailCall {..} =>
                    unreachable!()
            }

            Ok(Continue::Stmt)
        }

        fn verify_transfer(bp: &[usize], amt: &mut AbstractMutator, transfer: &Transfer)
            -> Result<(), IndexedErr<Vec<usize>>>
        {
            match transfer {
                &Transfer::Goto {dest: _} => (), // Branching handled by the RPO looping

                &Transfer::If {conseq: _, alt: _} => { // Branching handled by the RPO looping
                    amt.pop()?;
                },

                &Transfer::Ret =>
                    if amt.regs.len() < 2 { // Not even 'self' closure and return value
                        return Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(bp, amt.pc)});
                    },

                &Transfer::TailCall {argc} =>
                    if amt.regs.len() < argc {
                        return Err(IndexedErr {err: Err::RegsUnderflow, byte_index: byte_path(bp, amt.pc)});
                    },

                &Transfer::TailCallWithValues =>
                    return Err(IndexedErr {err: Err::UnexpectedInstr, byte_index: byte_path(bp, amt.pc)})
            }

            Ok(())
        }

        let cfg = CFG::try_from(code)
            .map_err(|err| IndexedErr {err: err.err, byte_index: byte_path(bp, err.byte_index)})?;

        let mut amts = HashMap::new();
        // DAG, so one pass in topological order is sufficient:
        for leader_index in cfg.post_order().iter().rev() {
            let block = cfg.blocks.get(leader_index).unwrap();

            let mut amt = if *leader_index == 0 { // entry block
                let mut amt = AbstractMutator::new(cfg.max_regs, bp);
                amt.push(AbstractType::Closure {len: cfg.clovers_len})?; // 'self' closure
                for _ in 1..cfg.arity { amt.push(AbstractType::Any)?; } // args
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

            let mut stmts = block.stmts.iter();
            loop {
                match verify_stmt(mt, code, bp, clovers, &cfg, &mut amt, &mut stmts, &block.transfer)? {
                    Continue::Stmt => (),
                    Continue::Transfer => {
                        verify_transfer(bp, &mut amt, &block.transfer)?;
                        break;
                    },
                    Continue::Block => break
                }
            }

            amts.insert(leader_index, amt);
        }

        Ok(())
    }

    verify_in(mt, &[], &[], code)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AbstractType {
    Bytecode,
    Closure {len: usize},
    CaseFn,
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

            CaseFn => match *other {
                CaseFn => CaseFn,
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
    consts: Gc<Vector<ORef>>,
    blocks: HashMap<usize, Block<'a>>
}

struct Block<'a> {
    predecessors: HashSet<usize>,
    stmts: Vec<DecodedInstr<'a>>,
    transfer: Transfer
}

#[derive(Clone, Copy)]
enum Transfer {
    Goto {dest: usize},
    If {conseq: usize, alt: usize},
    Ret,
    TailCall {argc: usize},
    TailCallWithValues
}

impl<'a> Block<'a> {
    fn successors(&self) -> Successors {
        match self.transfer {
            Transfer::Goto {dest} => Successors::new([dest, 0], 1),
            Transfer::If {conseq, alt} => Successors::new([conseq, alt], 2),
            Transfer::Ret | Transfer::TailCall {..} | Transfer::TailCallWithValues => Successors::new([0, 0], 0)
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
        use DecodedInstr::*;

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

            let (instr, instr_min_len) = try_decode(&instrs, i)?;

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

                TailCallWithValues => {
                    i += instr_min_len;
                    prev_was_branch = true;

                    blocks.insert(block_start, Block {
                        predecessors: mem::take(&mut predecessors),
                        stmts: mem::take(&mut stmts),
                        transfer: Transfer::TailCallWithValues
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

                Define{..} | GlobalSet{..} | Global{..} | Pop | Const{..} | Local{..} | Clover{..}
                | Box | UninitializedBox | BoxSet | CheckedBoxSet | BoxGet | CheckedBoxGet | CheckUse | Fn{..}
                | DomainFn {..} | CaseFn {..} | CheckOneReturnValue | IgnoreReturnValues => {
                    i += instr_min_len;
                    if i >= instrs.len() { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: i}); }

                    prev_was_branch = false;

                    stmts.push(instr);
                }

            }
        }

        if blocks.len() == 0 { return Err(IndexedErr{err: Err::MissingTerminator, byte_index: 0}); }

        Ok(CFG {
            arity: code.min_arity + code.varargs as usize,
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

fn try_decode<'a>(bytes: &'a [u8], mut i: usize) -> Result<(DecodedInstr<'a>, usize), IndexedErr<usize>> {
    if let Some(byte) = bytes.get(i) {
        match Opcode::try_from(*byte) {
            Ok(op) => {
                i += 1;

                match op {
                    Opcode::Define =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::Define {sym_index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::GlobalSet =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::GlobalSet {sym_index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})                            },

                    Opcode::Global =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::Global {sym_index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Const =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::Const {index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Local =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::Local {index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Clover =>
                        match bytes.get(i) {
                            Some(index) => Ok((DecodedInstr::Clover {index: *index as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Pop => Ok((DecodedInstr::Pop, 1)),

                    Opcode::Prune =>
                        match bytes.get(i) {
                            Some(prunes) => Ok((DecodedInstr::Prune {prunes: prunes}, 1)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Box => Ok((DecodedInstr::Box, 1)),
                    Opcode::UninitializedBox => Ok((DecodedInstr::UninitializedBox, 1)),
                    Opcode::BoxSet => Ok((DecodedInstr::BoxSet, 1)),
                    Opcode::CheckedBoxSet => Ok((DecodedInstr::CheckedBoxSet, 1)),
                    Opcode::BoxGet => Ok((DecodedInstr::BoxGet, 1)),
                    Opcode::CheckedBoxGet => Ok((DecodedInstr::CheckedBoxGet, 1)),
                    Opcode::CheckUse => Ok((DecodedInstr::CheckUse, 1)),

                    Opcode::Brf =>
                        match bytes.get(i) {
                            Some(dist) => Ok((DecodedInstr::Brf {dist: *dist as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },
                    Opcode::Br =>
                        match bytes.get(i) {
                            Some(dist) => Ok((DecodedInstr::Br {dist: *dist as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Fn =>
                        match bytes.get(i) {
                            Some(code_index) => {
                                i+= 1;

                                match bytes.get(i) {
                                    Some(len) => Ok((DecodedInstr::Fn {
                                        code_index: *code_index as usize,
                                        len: *len as usize
                                    }, 3)),
                                    None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                                }
                            },
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },
                    Opcode::DomainFn =>
                        match bytes.get(i) {
                            Some(arity) => {
                                i+= 1;

                                match bytes.get(i) {
                                    Some(code_index) => {
                                        i+= 1;

                                        match bytes.get(i) {
                                            Some(len) => Ok((DecodedInstr::DomainFn {
                                                arity: *arity as usize,
                                                code_index: *code_index as usize,
                                                len: *len as usize
                                            }, 4)),
                                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                                        }
                                    },

                                    None => Err(IndexedErr {err: Err::MissingOpcodeArg(op), byte_index: i})
                                }
                            },
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },
                    Opcode::CaseFn =>
                        match bytes.get(i) {
                            Some(clausec) => Ok((DecodedInstr::CaseFn {clausec: *clausec as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Call =>
                        match bytes.get(i) {
                            Some(argc) => {
                                i += 1;

                                match bytes.get(i) {
                                    Some(prunes) => Ok((DecodedInstr::Call {argc: *argc as usize, prunes: prunes}, 2)),
                                    None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                                }
                            },
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::CheckOneReturnValue => Ok((DecodedInstr::CheckOneReturnValue, 1)),
                    Opcode::IgnoreReturnValues => Ok((DecodedInstr::IgnoreReturnValues, 1)),
                    Opcode::TailCallWithValues => Ok((DecodedInstr::TailCallWithValues, 1)),

                    Opcode::TailCall =>
                        match bytes.get(i) {
                            Some(argc) => Ok((DecodedInstr::TailCall {argc: *argc as usize}, 2)),
                            None => Err(IndexedErr{err: Err::MissingOpcodeArg(op), byte_index: i})
                        },

                    Opcode::Ret => Ok((DecodedInstr::Ret, 1))
                }
            },

            Err(()) => Err(IndexedErr {err: Err::InvalidOpcode(*byte), byte_index: i})
        }
    } else {
        Err(IndexedErr {err: Err::MissingTerminator, byte_index: i})
    }
}
