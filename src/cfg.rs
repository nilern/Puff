use std::fmt;

use crate::oref::{WithinMt, DisplayWithin};
use crate::handle::Handle;
use crate::mutator::Mutator;

pub type Label = usize;

pub enum Instr {
    Const(Handle),
    Local(usize),
    Clover(usize),
    PopNNT(usize),
    Prune(Vec<bool>),
    If(Label, Label),
    Goto(Label),
    Code(Fn),
    Closure(usize),
    Call(usize, Vec<bool>),
    TailCall(usize),
    Ret
}

impl Instr {
    pub fn fmt(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        use Instr::*;

        match self {
            &Const(ref c) => writeln!(fmt, "{}const {}", indent, c.within(mt)),
            &Local(reg) => writeln!(fmt, "{}local {}", indent, reg),
            &Clover(i) => writeln!(fmt, "{}clover {}", indent, i),

            &PopNNT(n) => writeln!(fmt, "{}popnnt {}", indent, n),
            &Prune(ref prunes) => {
                write!(fmt, "{}prune #b", indent)?;
                for &prune in prunes { write!(fmt, "{}", prune as u8)?; }
                writeln!(fmt, "")
            }

            &If(conseq, alt) => writeln!(fmt, "{}if {} {}", indent, conseq, alt),
            &Goto(dest) => writeln!(fmt, "{}goto {}", indent, dest),

            &Code(ref code) => {
                writeln!(fmt, "{}code", indent)?;
                code.fmt(mt, fmt, &(indent.to_string() + "  "))
            },
            &Closure(len) => writeln!(fmt, "{}closure {}", indent, len),

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

pub type Block = Vec<Instr>;

pub struct Fn {
    pub arity: usize,
    pub blocks: Vec<Block>
}

impl DisplayWithin for &Fn {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result { self.fmt(mt, fmt, "") }
}

impl Fn {
    pub fn new(arity: usize) -> Self { Fn { arity, blocks: Vec::new() } }

    pub fn create_block(&mut self) -> Label {
        let label = self.blocks.len();
        self.blocks.push(Vec::new());
        label
    }

    pub fn insert_prune(&mut self, label: Label, prunes: Vec<bool>) {
        let block = &mut self.blocks[label];
        block.insert(block.len() - 1, Instr::Prune(prunes));
    }

    pub fn successors(&self, label: Label) -> Successors {
        match self.blocks[label].last().unwrap() {
            Instr::If(conseq, alt) => Successors::new([*conseq, *alt], 2),
            Instr::Goto(succ) => Successors::new([*succ, 0], 1),
            _ => Successors::new([0, 0], 0)
        }
    }

    pub fn fmt(&self, mt: &Mutator, fmt: &mut fmt::Formatter, indent: &str) -> fmt::Result {
        write!(fmt, "{}(", indent)?;
        if self.arity > 0 {
            write!(fmt, "_")?;

            for _ in 1..self.arity {
                write!(fmt, " _")?;
            }
        }
        writeln!(fmt, ")")?;

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
