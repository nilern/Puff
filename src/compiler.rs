use std::collections::hash_map::HashMap;

use crate::bytecode::Bytecode;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::anf;
use crate::cfg;
use crate::analyzer::analyze;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let anf = analyze(&mut cmp, expr);
    let cfg = cfg::Fn::from(&anf);
    println!("{}", cfg.within(cmp.mt));
    Gc::<Bytecode>::from_cfg(&mut cmp, &cfg)
}

pub struct Compiler<'a> {
    pub mt: &'a mut Mutator,
    name_counter: usize,
    names: HashMap<anf::Id, HandleT<Symbol>>
}

impl anf::Id {
    pub fn fresh(cmp: &mut Compiler) -> Self {
        let i = cmp.name_counter;
        cmp.name_counter = i + 1;
        Self::from(i)
    }
}

impl<'a> Compiler<'a> {
    fn new(mt: &'a mut Mutator) -> Self {
        Self {
            mt,
            name_counter: 0,
            names: HashMap::new()
        }
    }
}
