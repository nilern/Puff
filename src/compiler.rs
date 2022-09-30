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

    let mut anf = cmp.analyze(expr);
    Compiler::liveness(&mut anf);
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

type LiveVars = HashSet<Id>;

mod anf {
    use super::*;

    pub enum Triv {
        Use(Id),
        Const(Handle)
    }

    pub enum Expr {
        Let(Vec<Binding>, Box<Expr>),
        If(Box<Expr>, Box<Expr>, Box<Expr>),
        r#Fn(LiveVars, Params, Box<Expr>),
        Triv(Triv)
    }

    pub type Binding = (Id, Expr);

    pub type Params = Vec<Id>;
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
                                return analyze_let(cmp, env, unsafe { ls.as_ref().cdr });
                            } else if unsafe { callee.as_ref().name() == "if" } {
                                return analyze_if(cmp, env, unsafe { ls.as_ref().cdr });
                            } else if unsafe { callee.as_ref().name() == "fn" } {
                                return analyze_fn(cmp, env, unsafe { ls.as_ref().cdr });
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
                    (env, anf_bs)
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

        fn analyze_fn(cmp: &mut Compiler, env: &Rc<Env>, args: ORef) -> anf::Expr {
            // TODO: Reject duplicate parameter names:
            fn analyze_params(cmp: &mut Compiler, env: &Rc<Env>, mut params: ORef) -> (Rc<Env>, anf::Params) {
                let mut anf_ps = vec![Id::fresh(cmp)]; // Id for "self" closure
                let mut env = env.clone();

                while let Some(ps_pair) = params.try_cast::<Pair>(cmp.mt) {
                    if let Some(param) = unsafe { ps_pair.as_ref().car }.try_cast::<Symbol>(cmp.mt) {
                        let id = Id::fresh(cmp);
                        anf_ps.push(id);
                        env = Rc::new(Env::Binding(id, cmp.mt.root_t(param), env.clone()));
                    } else {
                        todo!()
                    }

                    params = unsafe { ps_pair.as_ref().cdr };
                }

                if params == EmptyList::instance(cmp.mt).into() {
                    (env, anf_ps)
                } else {
                    todo!()
                }
            }

            if let Some(args) = args.try_cast::<Pair>(cmp.mt) {
                let params = unsafe { args.as_ref().car };

                if let Some(args) = unsafe { args.as_ref().cdr }.try_cast::<Pair>(cmp.mt) {
                    let body = unsafe { args.as_ref().car };

                    if unsafe { args.as_ref().cdr } == EmptyList::instance(cmp.mt).into() {
                        let (env, params) = analyze_params(cmp, env, params);
                        let body = analyze_expr(cmp, &env, body);

                        r#Fn(LiveVars::new(), params, Box::new(body))
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

    fn liveness(expr: &mut anf::Expr) {
        use anf::Expr::{self, *};
        use anf::Triv::*;

        fn live_ins(expr: &mut Expr, mut live_outs: LiveVars) -> LiveVars {
            match expr {
                &mut Let(ref mut bindings, ref mut body) => {
                    live_outs = live_ins(body, live_outs);

                    for (id, val) in bindings.iter_mut().rev() {
                        live_outs.remove(id);
                        live_outs = live_ins(val, live_outs);
                    }
                },

                &mut If(ref mut cond, ref mut conseq, ref mut alt) => {
                    let conseq_outs = live_outs.clone();
                    let alt_ins = live_ins(alt, live_outs);
                    let mut conseq_ins = live_ins(conseq, conseq_outs);

                    conseq_ins.extend(alt_ins);
                    live_outs = live_ins(cond, conseq_ins);
                },

                &mut r#Fn(ref mut fvs, ref params, ref mut body) => {
                    let mut free_vars = live_ins(body, LiveVars::new());

                    for param in params {
                        free_vars.remove(param);
                    }

                    fvs.extend(free_vars.iter());

                    live_outs.extend(free_vars);
                },

                &mut Triv(Use(id)) => { live_outs.insert(id); },

                &mut Triv(Const(_)) => ()
            }

            live_outs
        }

        live_ins(expr, LiveVars::new());
    }

    fn emit(&mut self, expr: &anf::Expr) -> Gc<Bytecode> {
        use anf::Expr::*;
        use anf::Triv::*;

        enum Loc {
            Reg(usize),
            Clover(usize)
        }

        struct Locals {
            id: Id, 
            reg: usize,
            parent: Option<Rc<Locals>>
        }

        impl Locals {
            fn get(locals: &Option<Rc<Locals>>, id: Id) -> Option<usize> {
                let mut locals = locals.clone();
                loop {
                    if let Some(locs) = locals {
                        match *locs {
                            Self {id: rid, reg, parent: _} if rid == id => return Some(reg),
                            Self {ref parent, ..} => locals = parent.clone()
                        }
                    } else {
                        return None;
                    }
                }
            }
        }

        #[derive(Clone)]
        struct Env {
            clovers: Rc<HashMap<Id, usize>>,
            params: Rc<HashMap<Id, usize>>,
            locals: Option<Rc<Locals>>,
            len: usize
        }

        impl Env {
            fn r#fn(clover_ids: &[Id], param_ids: &[Id]) -> Self {
                let mut clovers = HashMap::new();
                for (i, &id) in clover_ids.iter().enumerate() {
                    clovers.insert(id, i);
                }

                let mut params = HashMap::new();
                for (i, &id) in param_ids.iter().enumerate() {
                    params.insert(id, i);
                }

                Self {
                    clovers: Rc::new(clovers),
                    params: Rc::new(params),
                    locals: None,
                    len: param_ids.len()
                }
            }

            fn r#let(parent: &Env, id: Id) -> Self {
                let reg = parent.len;
                Self {
                    clovers: parent.clovers.clone(),
                    params: parent.params.clone(),
                    locals: Some(Rc::new(Locals {id, reg, parent: parent.locals.clone()})),
                    len: reg.checked_add(1).unwrap()
                }
            }

            fn get(&self, id: Id) -> Loc {
                Locals::get(&self.locals, id).map(Loc::Reg)
                    .or_else(|| self.params.get(&id).map(|&reg| Loc::Reg(reg)))
                    .or_else(|| self.clovers.get(&id).map(|&i| Loc::Clover(i)))
                    .unwrap()
            }
        }

        fn emit_use(builder: &mut bytecode::Builder, env: &Env, r#use: Id) {
            match env.get(r#use) {
                Loc::Reg(reg) => builder.local(reg),
                Loc::Clover(i) => builder.clover(i)
            }
        }

        fn emit_expr(cmp: &mut Compiler, builder: &mut bytecode::Builder, env: &Env, tail: bool,
            expr: &anf::Expr)
        {
            match *expr {
                Let(ref bindings, ref body) => {
                    let mut env = env.clone();
                    for &(id, ref val) in bindings {
                        emit_expr(cmp, builder, &env, false, val);
                        env = Env::r#let(&env, id);
                    }

                    emit_expr(cmp, builder, &env, tail, &**body);

                    let nbs = bindings.len();
                    if !tail && nbs > 0 { builder.popnnt(nbs); }
                },

                If(ref cond, ref conseq, ref alt) => {
                    emit_expr(cmp, builder, env, false, cond);
                    let brf_i = builder.brf();

                    emit_expr(cmp, builder, env, tail, conseq);
                    let br_i = if tail { None } else { Some(builder.br()) };

                    builder.backpatch(brf_i);

                    emit_expr(cmp, builder, env, tail, alt);

                    if let Some(br_i) = br_i { builder.backpatch(br_i); }
                },

                Fn(ref fvs, ref params, ref body) => {
                    let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();

                    let code = {
                        let code = emit_fn(cmp, &fvs, params, body);
                        cmp.mt.root_t(code)
                    };
                    builder.r#const(cmp.mt, (*code).into());

                    for &fv in fvs.iter() {
                        emit_use(builder, &env, fv);
                    }

                    builder.r#fn(fvs.len());

                    if tail { builder.ret(); }
                },

                Triv(Use(id)) => {
                    emit_use(builder, env, id);
                    if tail { builder.ret(); }
                },

                Triv(Const(ref c)) => {
                    builder.r#const(cmp.mt, **c);
                    if tail { builder.ret(); }
                }
            }
        }

        fn emit_fn(cmp: &mut Compiler, clovers: &[Id], params: &[Id], body: &anf::Expr) -> Gc<Bytecode> {
            let mut builder = bytecode::Builder::new();

            emit_expr(cmp, &mut builder, &Rc::new(Env::r#fn(clovers, params)), true, body);

            builder.build(cmp.mt)
        }

        emit_fn(self, &[], &[], expr)
    }
}
