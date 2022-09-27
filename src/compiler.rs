use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::rc::Rc;

use crate::bytecode::{self, Bytecode};
use crate::oref::{ORef, Gc};
use crate::handle::{Handle, HandleT};
use crate::list::{Pair, EmptyList};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::heap_obj::Singleton;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let anf = cmp.analyze(expr);
    Compiler::liveness(&anf);
    cmp.emit(&anf)
}

struct Compiler<'a> {
    mt: &'a mut Mutator,
    name_counter: usize,
    names: HashMap<Id, HandleT<Symbol>>
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl Id {
    fn fresh(cmp: &mut Compiler) -> Self {
        let i = cmp.name_counter;
        cmp.name_counter = i + 1;
        Self(i)
    }
}

mod anf {
    use super::*;

    pub enum Triv {
        Use(Id),
        Const(Handle)
    }

    pub enum Expr {
        Let(Vec<Binding>, Box<Expr>),
        If(Box<Expr>, Box<Expr>, Box<Expr>),
        Triv(Triv)
    }

    pub type Binding = (Id, Expr);
}

impl<'a> Compiler<'a> {
    fn new(mt: &'a mut Mutator) -> Self {
        Self {
            mt,
            name_counter: 0,
            names: HashMap::new()
        }
    }

    fn analyze(&mut self, expr: ORef) -> anf::Expr {
        use anf::Expr::*;
        use anf::Triv::*;

        enum Env {
            Binding(Id, HandleT<Symbol>, Rc<Env>),
            Empty
        }

        impl Env {
            fn get(env: &Rc<Env>, name: Gc<Symbol>) -> Option<Id> {
                let mut env = env.clone();
                loop {
                    match &*env {
                        &Self::Binding(id, ref bname, ref parent) =>
                            if **bname == name {
                                return Some(id);
                            } else {
                                env = parent.clone();
                            },
                        &Self::Empty => return None
                    }
                }
            }
        }

        fn analyze_expr(cmp: &mut Compiler, env: &Rc<Env>, expr: ORef) -> anf::Expr {
            if let Ok(expr) = Gc::<()>::try_from(expr.clone()) {
                if let Some(name) = expr.try_cast::<Symbol>(cmp.mt) {
                    return Triv(Use(Env::get(env, name).unwrap()));
                } else if let Some(ls) = expr.try_cast::<Pair>(cmp.mt) {
                    let callee = unsafe { ls.as_ref().car };
                    if let Ok(callee) = Gc::<()>::try_from(callee) {
                        if let Some(callee) = callee.try_cast::<Symbol>(cmp.mt) {
                            if unsafe { callee.as_ref().name() == "let" } {
                                let args = unsafe { ls.as_ref().cdr };
                                return analyze_let(cmp, env, args);
                            } else if unsafe { callee.as_ref().name() == "if" } {
                                let args = unsafe { ls.as_ref().cdr };
                                return analyze_if(cmp, env, args);
                            }
                        }
                    }

                    todo!()
                }
            }

            Triv(Const(cmp.mt.root(expr)))
        }

        fn analyze_let(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
            fn analyze_bindings(cmp: &mut Compiler, env: &Rc<Env>, bindings: ORef) -> (Rc<Env>, Vec<anf::Binding>) {
                fn analyze_binding(cmp: &mut Compiler, env: &Rc<Env>, anf_bs: &mut Vec<anf::Binding>, binding: ORef)
                    -> Rc<Env>
                {

                    if let Some(binding) = binding.try_cast::<Pair>(cmp.mt) {
                        let pat = unsafe { binding.as_ref().car };

                        if let Some(binding) = unsafe { binding.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                            let val = unsafe { binding.as_ref().car };

                            if unsafe { binding.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                                if let Some(name) = pat.try_cast::<Symbol>(cmp.mt) {
                                    let id = Id::fresh(cmp);
                                    anf_bs.push((id, analyze_expr(cmp, &env, val)));
                                    Rc::new(Env::Binding(id, cmp.mt.root_t(name), env.clone()))
                                } else {
                                    todo!()
                                }
                            } else {
                                todo!()
                            }
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                }

                let mut anf_bs = Vec::new();

                let mut env = env.clone();
                let mut bs = bindings;
                while let Some(bs_pair) = bs.try_cast::<Pair>(cmp.mt) {
                    env = analyze_binding(cmp, &env, &mut anf_bs, unsafe { bs_pair.as_ref().car });
                    bs = unsafe { bs_pair.as_ref().cdr };
                }

                if bs == EmptyList::instance(cmp.mt).into() {
                    (env.clone(), anf_bs)
                } else {
                    todo!()
                }
            }

            if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
                let bindings = unsafe { args.as_ref().car };
                let args = unsafe { args.as_ref().cdr };

                if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
                    let body = unsafe { args.as_ref().car };
                    let args = unsafe { args.as_ref().cdr };

                    if args == EmptyList::instance(cmp.mt).into() {
                        let (env, bindings) = analyze_bindings(cmp, env, bindings);
                        let body = analyze_expr(cmp, &env, body);
                        Let(bindings, Box::new(body))
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }

        fn analyze_if(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
            if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
                let cond = analyze_expr(cmp, env, unsafe { args.as_ref().car });

                let branches = unsafe { args.as_ref().cdr };
                if let Some(branches) = branches.try_cast::<Pair>(cmp.mt) {
                    let conseq = analyze_expr(cmp, env, unsafe { branches.as_ref().car });

                    let branches = unsafe { branches.as_ref().cdr };
                    if let Some(branches) = branches.try_cast::<Pair>(cmp.mt) {
                        let alt = analyze_expr(cmp, env, unsafe { branches.as_ref().car });

                        let branches = unsafe { branches.as_ref().cdr };
                        if branches == EmptyList::instance(cmp.mt).into() {
                            If(Box::new(cond), Box::new(conseq), Box::new(alt))
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        }

        analyze_expr(self, &Rc::new(Env::Empty), expr)
    }

    fn liveness(expr: &anf::Expr) {
        use anf::Expr::{self, *};
        use anf::Triv::*;

        type LiveVars = HashSet<Id>;

        fn live_ins(expr: &Expr, mut live_outs: LiveVars) -> LiveVars {
            match expr {
                &Let(ref bindings, ref body) => {
                    live_outs = live_ins(&body, live_outs);

                    for (id, val) in bindings.iter().rev() {
                        live_outs.remove(id);
                        live_outs = live_ins(val, live_outs);
                    }
                },

                &If(ref cond, ref conseq, ref alt) => {
                    let conseq_outs = live_outs.clone();
                    let alt_ins = live_ins(alt, live_outs);
                    let mut conseq_ins = live_ins(conseq, conseq_outs);

                    conseq_ins.extend(alt_ins);
                    live_outs = live_ins(cond, conseq_ins);
                },

                &Triv(Use(id)) => { live_outs.insert(id); },

                &Triv(Const(_)) => ()
            }

            live_outs
        }

        live_ins(expr, LiveVars::new());
    }

    fn emit(&mut self, expr: &anf::Expr) -> Gc<Bytecode> {
        use anf::Expr::*;
        use anf::Triv::*;

        enum Regs {
            Binding {
                id: Id, 
                reg: u8,
                parent: Rc<Regs>,
                len: u8
            },
            Empty
        }

        impl Regs {
            fn new() -> Self { Self::Empty }

            fn with(regs: &Rc<Regs>, id: Id) -> Self {
                let reg = regs.len();
                Self::Binding {
                    id,
                    reg,
                    parent: regs.clone(),
                    len: reg.checked_add(1).unwrap()
                }
            }

            fn get(regs: &Rc<Regs>, id: Id) -> u8 {
                let mut regs = regs.clone();
                loop {
                    match &*regs {
                        &Self::Binding {id: rid, reg, ref parent, len: _} =>
                            if rid == id {
                                return reg;
                            } else {
                                regs = parent.clone();
                            }
                        &Self::Empty => unreachable!()
                    }
                }
            }

            fn len(&self) -> u8 {
                match self {
                    &Self::Binding {len, ..} => len,
                    &Self::Empty => 0
                }
            }
        }

        fn emit_expr(mt: &mut Mutator, builder: &mut bytecode::Builder, regs: &Rc<Regs>, tail: bool,
            expr: &anf::Expr)
        {
            match *expr {
                Let(ref bindings, ref body) => {
                    let mut regs = regs.clone();
                    for &(id, ref val) in bindings {
                        emit_expr(mt, builder, &regs, false, val);
                        regs = Rc::new(Regs::with(&regs, id));
                    }

                    emit_expr(mt, builder, &regs, tail, &**body);

                    if let Ok(nbs) = u8::try_from(bindings.len()) {
                        if !tail && nbs > 0 { builder.popnnt(nbs); }
                    } else {
                        todo!()
                    }
                },

                If(ref cond, ref conseq, ref alt) => {
                    emit_expr(mt, builder, regs, false, cond);
                    let brf_i = builder.brf();

                    emit_expr(mt, builder, regs, tail, conseq);
                    let br_i = if tail { None } else { Some(builder.br()) };

                    builder.backpatch(brf_i);

                    emit_expr(mt, builder, regs, tail, alt);

                    if let Some(br_i) = br_i { builder.backpatch(br_i); }
                },

                Triv(Use(id)) => {
                    builder.local(Regs::get(regs, id));
                    if tail { builder.ret(); }
                },

                Triv(Const(ref c)) => {
                    builder.r#const(mt, **c);
                    if tail { builder.ret(); }
                }
            }
        }

        let mut builder = bytecode::Builder::new();

        emit_expr(self.mt, &mut builder, &Rc::new(Regs::new()), true, expr);

        builder.build(self.mt)
    }
}
