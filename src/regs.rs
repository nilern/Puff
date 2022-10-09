use crate::mutator::Mutator;
use crate::oref::ORef;

pub struct Regs {
    regs: Vec<ORef>,
    start: usize
}

impl Regs {
    pub fn new() -> Self {
        Self {
            regs: Vec::new(),
            start: 0
        }
    }

    pub fn as_slice(&self) -> &[ORef] { &self.regs[self.start..] }

    pub fn as_mut_slice(&mut self) -> &mut [ORef] { &mut self.regs[self.start..] }

    pub fn pop(&mut self) -> Option<ORef> { self.regs.pop() }

    pub fn popn(&mut self, n: usize) { self.regs.truncate(self.regs.len() - n); }

    pub fn popnnt(&mut self, n: usize) {
        let len = self.regs.len();
        let top = self.regs[len - 1];
        let new_len = len - n as usize;
        self.regs.truncate(new_len);
        self.regs[new_len - 1] = top;
    }

    pub fn push(&mut self, v: ORef) { self.regs.push(v) }

    pub fn enter(&mut self, new_len: usize) {
        self.start = self.regs.len() - new_len;
    }

    pub fn extend(&mut self, vs: &[ORef]) { self.regs.extend(vs); }

    pub fn truncate(&mut self, n: usize) { self.regs.truncate(self.start + n); }

    pub fn dump(&self, mt: &Mutator) {
        print!("[");
        for v in self.as_slice() { print!("{}, ", v.within(mt)); }
        println!("]");
    }
}
