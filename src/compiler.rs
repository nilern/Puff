use std::collections::hash_map::HashMap;
use std::io::stdout;
use pretty::RcDoc;

use crate::bytecode::Bytecode;
use crate::oref::{ORef, Gc};
use crate::handle::HandleT;
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::cfg;
use crate::analyzer::analyze;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl Id {
    pub fn src_fresh(cmp: &mut Compiler, name: HandleT<Symbol>) -> Self {
        let id = Self::fresh(cmp);
        cmp.names.insert(id, name);
        id
    }

    pub fn fresh(cmp: &mut Compiler) -> Self {
        let i = cmp.name_counter;
        cmp.name_counter = i + 1;
        Self(i)
    }

    pub fn freshen(cmp: &mut Compiler, old_id: Self) -> Self {
        let id = Self::fresh(cmp);

        if let Some(name) = cmp.names.get(&old_id) {
            cmp.names.insert(id, name.clone());
        }

        id
    }

    pub fn to_doc<'a>(self, cmp: &Compiler) -> RcDoc<'a, ()> {
        RcDoc::text(match cmp.names.get(&self) {
            Some(sym) => unsafe { format!("{}${}", sym.as_ref().name(), self.0) },
            None => format!("${}", self.0)
        })
    }
}

pub struct Compiler<'a> {
    pub mt: &'a mut Mutator,
    name_counter: usize,
    names: HashMap<Id, HandleT<Symbol>>
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

pub fn compile(mt: &mut Mutator, expr: ORef, debug: bool) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let anf = analyze(&mut cmp, expr);
    if debug {
        anf.to_doc(cmp.mt, &cmp).render(80, &mut stdout());
        println!("");
    }

    let cfg = cfg::Fn::from(&anf);
    if debug {
        println!("{}", cfg.within(cmp.mt));
    }
    
    Gc::<Bytecode>::from_cfg(&mut cmp, &cfg)
}
