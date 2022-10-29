use std::collections::hash_map::HashMap;
use std::io::stdout;
use pretty::RcDoc;

mod anf;
mod analyzer;
mod mutables;
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

    let mut anf = analyze(&mut cmp, expr);
    if debug {
        anf.to_doc(cmp.mt, &cmp).render(80, &mut stdout());
        println!("");
    }

    let muts = mutables(&anf);

    anf = letrec(&mut cmp, &muts, &anf);

    anf = convert_mutables(&mut cmp, &muts, &anf); // `muts` should not have been invalidated by `letrec`

    liveness(&mut anf);

    let cfg = cfg::Fn::from(&anf);
    if debug {
        println!("{}", cfg.within(cmp.mt));
    }
    
    emit(&mut cmp, &cfg)
}

fn liveness(expr: &mut anf::Expr) {
    use anf::Expr::{self, *};
    use anf::Triv::*;

    fn live_ins(expr: &mut Expr, mut live_outs: anf::LiveVars) -> anf::LiveVars {
        match expr {
            &mut Define(_, ref mut val_expr) => live_ins(val_expr, live_outs),
            &mut GlobalSet(_, ref mut val_expr) => live_ins(val_expr, live_outs),

            &mut Begin(ref mut stmts) =>
                stmts.iter_mut().rev()
                    .fold(live_outs, |live_outs, stmt| live_ins(stmt, live_outs)),

            &mut Let(ref mut bindings, ref mut body, _) => {
                let body_live_ins = live_ins(body, live_outs);

                bindings.iter_mut().rev()
                    .fold(body_live_ins, |mut live_outs, (id, val_expr)| {
                        live_outs.remove(id);
                        live_ins(val_expr, live_outs)
                    })
            },

            &mut If(ref mut cond, ref mut conseq, ref mut alt, ref mut if_live_outs) => {
                *if_live_outs = live_outs.iter().copied().collect();

                let conseq_outs = live_outs.clone();
                let alt_ins = live_ins(alt, live_outs);
                let mut conseq_ins = live_ins(conseq, conseq_outs);

                conseq_ins.extend(alt_ins);
                live_ins(cond, conseq_ins)
            },

            &mut Box(ref mut val_expr) => live_ins(val_expr, live_outs),

            &mut UninitializedBox => live_outs,

            &mut BoxSet(r#box, ref mut val_expr) => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins
            },

            &mut CheckedBoxSet {guard, r#box, ref mut val_expr} => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins.insert(guard);
                val_expr_live_ins
            },

            &mut BoxGet(r#box) => { live_outs.insert(r#box); live_outs }

            &mut CheckedBoxGet {guard, r#box} => {
                live_outs.insert(r#box);
                live_outs.insert(guard);
                live_outs
            }

            &mut r#Fn(ref mut fvs, ref params, _, ref mut body) => {
                let mut free_vars = {
                    let mut live_outs = anf::LiveVars::new();
                    live_outs.insert(params[0]); // "self" closure should always be live
                    live_ins(body, live_outs)
                };

                for param in params {
                    free_vars.remove(param);
                }

                *fvs = free_vars.iter().copied().collect();

                live_outs.extend(free_vars);
                live_outs
            },

            &mut Call(callee, ref args, ref mut call_live_outs) => {
                *call_live_outs = live_outs.iter().copied().collect();

                for &arg in args.iter().rev() {
                    live_outs.insert(arg);
                }

                live_outs.insert(callee);
                live_outs
            },

            &mut Global(_) => live_outs,

            &mut CheckedUse {guard, id} => {
                live_outs.insert(id);
                live_outs.insert(guard);
                live_outs
            },

            &mut Triv(Use(id)) => { live_outs.insert(id); live_outs },

            &mut Triv(Const(_)) => live_outs,

            &mut Letrec(..) | &mut Set(..) => unreachable!()
        }
    }

    live_ins(expr, anf::LiveVars::new());
}
