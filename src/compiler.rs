use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;

use crate::bytecode::{self, Bytecode};
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
    cmp.emit(&cfg)
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

    fn emit(&mut self, f: &cfg::Fn) -> Gc<Bytecode> {
        use cfg::Instr::*;

        fn cfg_po(f: &cfg::Fn) -> Vec<cfg::Label> {
            fn block_po(f: &cfg::Fn, label: cfg::Label, visited: &mut HashSet<cfg::Label>, po: &mut Vec<cfg::Label>) {
                if !visited.contains(&label) {
                    visited.insert(label);

                    for succ in f.successors(label).rev() { // In reverse so that `brf` can fall through
                        block_po(f, succ, visited, po);
                    }

                    po.push(label);
                }
            }

            let mut po = Vec::new();
            let mut visited = HashSet::new();
            block_po(f, 0, &mut visited, &mut po);
            po
        }

        fn emit_instr(cmp: &mut Compiler, builder: &mut bytecode::Builder, instr: &cfg::Instr,
            rpo_next: Option<cfg::Label>
        ) {
            match instr {
                &Const(ref c) => builder.r#const(cmp.mt, **c),
                &Local(reg) => builder.local(reg),
                &Clover(i) => builder.clover(i),

                &PopNNT(n) => builder.popnnt(n),
                &Prune(ref prunes) => builder.prune(prunes),

                &If(_, alt) => builder.brf(alt),
                &Goto(dest) => if dest != rpo_next.unwrap() { builder.br(dest) },

                &Code(ref code) => {
                    let code = emit_fn(cmp, code).into();
                    builder.r#const(cmp.mt, code);
                },
                &Closure(len) => builder.r#fn(len),

                &Call(argc, ref prunes) => builder.call(argc, prunes),
                &TailCall(argc) => builder.tailcall(argc),
                &Ret => builder.ret()
            }
        }

        fn emit_block(cmp: &mut Compiler, builder: &mut bytecode::Builder, f: &cfg::Fn, label: cfg::Label,
            rpo_next: Option<cfg::Label>
        ) {
            builder.label(label);

            for instr in f.blocks[label].iter() {
                emit_instr(cmp, builder, instr, rpo_next);
            }
        }

        fn emit_fn(cmp: &mut Compiler, f: &cfg::Fn) -> Gc<Bytecode> {
            let po = cfg_po(f);

            let mut builder = bytecode::Builder::new(f.arity);

            let mut rpo = po.iter().rev().peekable();
            while let Some(&label) = rpo.next() {
                emit_block(cmp, &mut builder, f, label, rpo.peek().map(|&&label| label));
            }

            builder.build(cmp.mt)
        }

        emit_fn(self, f)
    }
}
