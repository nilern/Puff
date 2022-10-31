use std::fmt;
use std::collections::hash_set::HashSet;

use crate::oref::ORef;
use crate::handle::{Handle, HandleT};
use crate::mutator::{Mutator, WithinMt};
use crate::symbol::Symbol;
use crate::vector::Vector;
use crate::heap_obj::Indexed;
use crate::write::DisplayWithin;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(usize);

impl Label {
    const ENTRY: Self = Label(0);
}

impl fmt::Display for Label {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result { self.0.fmt(fmt) }
}

pub enum Instr {
    Define(HandleT<Symbol>),
    GlobalSet(HandleT<Symbol>),
    Global(HandleT<Symbol>),

    Const(Handle),
    Local(usize),
    Clover(usize),

    PopNNT(usize),
    Prune(Vec<bool>),

    Box,
    UninitializedBox,
    BoxSet,
    CheckedBoxSet,
    BoxGet,
    CheckedBoxGet,
    CheckUse,

    If(Label, Label),
    Goto(Label),

    Fn(Fn, usize),
    Call(usize, Vec<bool>),
    TailCall(usize),
    Ret
}

impl Instr {
    pub fn fmt(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        use Instr::*;

        match self {
            &Define(ref definiend) => unsafe { writeln!(fmt, "{}define {}", indent, definiend.as_ref().name()) },
            &GlobalSet(ref name) => unsafe { writeln!(fmt, "{}global-set! {}", indent, name.as_ref().name()) },
            &Global(ref name) => unsafe { writeln!(fmt, "{}global {}", indent, name.as_ref().name()) },

            &Const(ref c) => writeln!(fmt, "{}const {}", indent, c.within(mt)),
            &Local(reg) => writeln!(fmt, "{}local {}", indent, reg),
            &Clover(i) => writeln!(fmt, "{}clover {}", indent, i),

            &PopNNT(n) => writeln!(fmt, "{}popnnt {}", indent, n),
            &Prune(ref prunes) => {
                write!(fmt, "{}prune #b", indent)?;
                for &prune in prunes { write!(fmt, "{}", prune as u8)?; }
                writeln!(fmt, "")
            }

            &Box => writeln!(fmt, "{}box", indent),
            &UninitializedBox => writeln!(fmt, "{}uninitialized-box", indent),
            &BoxSet => writeln!(fmt, "{}box-set!", indent),
            &CheckedBoxSet => writeln!(fmt, "{}checked-box-set!", indent),
            &BoxGet => writeln!(fmt, "{}box-get", indent),
            &CheckedBoxGet => writeln!(fmt, "{}checked-box-get", indent),
            &CheckUse => writeln!(fmt, "{}check-use", indent),

            &If(conseq, alt) => writeln!(fmt, "{}if {} {}", indent, conseq, alt),
            &Goto(dest) => writeln!(fmt, "{}goto {}", indent, dest),

            &Fn(ref code, len) => {
                writeln!(fmt, "{}fn {}", indent, len)?;
                code.fmt(mt, fmt, &(indent.to_string() + "  "))
            },

            &Call(argc, ref prunes) => {
                write!(fmt, "{}call {} #b", indent, argc)?;
                for &prune in prunes { write!(fmt, "{}", prune as u8)?; }
                writeln!(fmt, "")
            },
            &TailCall(argc) => writeln!(fmt, "{}tailcall {}", indent, argc),
            &Ret => writeln!(fmt, "{}ret", indent)
        }
    }
}

pub struct PosInstr {
    pub pos: Handle,
    pub instr: Instr
}

impl PosInstr {
    pub fn fmt(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        self.instr.fmt(mt, fmt, indent)
    }
}

pub type Block = Vec<PosInstr>;

pub struct Fn {
    pub min_arity: usize,
    pub varargs: bool,
    pub max_regs: usize,
    pub clover_names: HandleT<Vector<ORef>>,
    pub blocks: Vec<Block>
}

impl DisplayWithin for &Fn {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result { self.fmt(mt, fmt, "") }
}

impl Fn {
    pub fn new(min_arity: usize, varargs: bool, max_regs: usize, clover_names: HandleT<Vector<ORef>>) -> Self {
        Fn { min_arity, varargs, max_regs, clover_names, blocks: Vec::new() }
    }

    pub fn block(&self, i: Label) -> &Block { &self.blocks[i.0] }

    pub fn block_mut(&mut self, i: Label) -> &mut Block { &mut self.blocks[i.0] }

    pub fn create_block(&mut self) -> Label {
        let label = Label(self.blocks.len());
        self.blocks.push(Vec::new());
        label
    }

    pub fn insert_prune(&mut self, label: Label, prunes: Vec<bool>, pos: Handle) {
        let block = self.block_mut(label);
        block.insert(block.len() - 1, PosInstr {instr: Instr::Prune(prunes), pos});
    }

    pub fn successors(&self, label: Label) -> Successors {
        match self.block(label).last().unwrap().instr {
            Instr::If(conseq, alt) => Successors::new([conseq, alt], 2),
            Instr::Goto(succ) => Successors::new([succ, Label::ENTRY], 1),
            _ => Successors::new([Label::ENTRY, Label::ENTRY], 0)
        }
    }

    pub fn post_order(&self) -> Vec<Label> {
        fn block_post_order(f: &Fn, label: Label, visited: &mut HashSet<Label>, post_order: &mut Vec<Label>) {
            if !visited.contains(&label) {
                visited.insert(label);

                for succ in f.successors(label).rev() { // In reverse so that `brf` can fall through
                    block_post_order(f, succ, visited, post_order);
                }

                post_order.push(label);
            }
        }

        let mut post_order = Vec::new();
        let mut visited = HashSet::new();
        block_post_order(self, Label::ENTRY, &mut visited, &mut post_order);
        post_order
    }

    pub fn fmt(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        write!(fmt, "{}(clovers {}) ", indent, unsafe { self.clover_names.as_ref().indexed_field().len() })?;
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

        for (label, block) in self.blocks.iter().enumerate() {
            writeln!(fmt, "{}{}:", indent, label)?;

            for instr in block.iter() {
                instr.fmt(mt, fmt, &(indent.to_string() + "  "))?;
            }
        }

        Ok(())
    }

    pub fn within<'a>(&'a self, mt: &'a Mutator) -> WithinMt<&'a Self> { WithinMt {v: self, mt} }
}

pub struct Successors {
    start: usize,
    end: usize,
    items: [Label; 2]
}

impl Successors {
    fn new(items: [Label; 2], len: usize) -> Self { Successors {start: 0, end: len, items} }
}

impl Iterator for Successors {
    type Item = Label;

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
