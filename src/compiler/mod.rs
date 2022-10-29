use std::collections::hash_map::HashMap;
use std::io::stdout;
use pretty::RcDoc;

mod anf;
mod analyzer;
mod mutables;
mod liveness;
pub mod cfg;
mod to_cfg;
mod emit;

use crate::bytecode::Bytecode;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use analyzer::analyze;
use mutables::{mutables, letrec, convert_mutables};
use liveness::liveness;
use emit::emit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl Id {
    pub fn src_fresh(cmp: &mut Compiler, name: HandleT<Symbol>) -> Self {
        let id = Self::fresh(cmp);
        cmp.names.insert(id, name);
        id
    }

    pub fn fresh(cmp: &mut Compiler) -> Self {
        let i = cmp.id_counter;
        cmp.id_counter = i + 1;
        Self(i)
    }

    pub fn freshen(cmp: &mut Compiler, old_id: Self) -> Self {
        let id = Self::fresh(cmp);

        if let Some(name) = cmp.names.get(&old_id) {
            cmp.names.insert(id, name.clone());
        }

        id
    }

    pub fn name(self, cmp: &Compiler) -> Option<HandleT<Symbol>> { cmp.names.get(&self).map(Clone::clone) }

    pub fn to_doc<'a>(self, cmp: &Compiler) -> RcDoc<'a, ()> {
        RcDoc::text(match cmp.names.get(&self) {
            Some(sym) => unsafe { format!("{}${}", sym.as_ref().name(), self.0) },
            None => format!("${}", self.0)
        })
    }
}

pub struct Compiler<'a> {
    pub mt: &'a mut Mutator,
    id_counter: usize,
    names: HashMap<Id, HandleT<Symbol>>
}

impl<'a> Compiler<'a> {
    fn new(mt: &'a mut Mutator) -> Self {
        Self {
            mt,
            id_counter: 0,
            names: HashMap::new()
        }
    }
}

pub fn compile(mt: &mut Mutator, expr: ORef, debug: bool) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let mut anf = analyze(&mut cmp, expr);
    if debug {
        anf.to_doc(cmp.mt, &cmp).render(80, &mut stdout());
        println!("\n");
    }

    let muts = mutables(&anf);

    anf = letrec(&mut cmp, &muts, &anf);

    anf = convert_mutables(&mut cmp, &muts, &anf); // `muts` should not have been invalidated by `letrec`

    if debug {
        anf.to_doc(cmp.mt, &cmp).render(80, &mut stdout());
        println!("\n");
    }

    liveness(&mut anf);

    let cfg = cfg::Fn::from_anf(&mut cmp, &anf);
    if debug {
        println!("{}", cfg.within(cmp.mt));
    }
    
    emit(&mut cmp, &cfg)
}
