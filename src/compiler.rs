use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::rc::Rc;
use std::fmt;

use crate::bytecode::{self, Bytecode};
use crate::oref::{WithinMt, DisplayWithin, ORef, Gc};
use crate::handle::{Handle, HandleT};
use crate::list::{Pair, EmptyList};
use crate::mutator::Mutator;
use crate::symbol::Symbol;
use crate::heap_obj::Singleton;

pub fn compile(mt: &mut Mutator, expr: ORef) -> Gc<Bytecode> {
    let mut cmp = Compiler::new(mt);

    let mut anf = cmp.analyze(expr);
    Compiler::liveness(&mut anf);
    let cfg = cmp.to_cfg(&anf);
    println!("{}", cfg.within(cmp.mt));
    cmp.emit(&cfg)
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
        Let(Vec<Binding>, Box<Expr>, /* popnnt?: */ bool),
        If(Box<Expr>, Box<Expr>, Box<Expr>, LiveVars),
        r#Fn(LiveVars, Params, Box<Expr>),
        Call(Id, Vec<Id>, LiveVars),
        Triv(Triv)
    }

    pub type Binding = (Id, Expr);

    pub type Params = Vec<Id>;
}

mod cfg {
    use super::*;

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

                    return analyze_call(cmp, env, ls);
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
                        Let(bindings, Box::new(body), true)
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
                            If(Box::new(cond), Box::new(conseq), Box::new(alt), LiveVars::new())
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

        fn analyze_call(cmp: &mut Compiler, env: &Rc<Env>, expr: Gc<Pair>) -> anf::Expr {
            let mut bindings = Vec::new();
            let mut arg_ids = Vec::new();

            let anf_callee = analyze_expr(cmp, env, unsafe { expr.as_ref().car });
            let callee_id = Id::fresh(cmp);
            bindings.push((callee_id, anf_callee));

            let mut args = unsafe { expr.as_ref().cdr };
            while let Some(args_pair) = args.try_cast::<Pair>(cmp.mt) {
                let anf_arg = analyze_expr(cmp, env, unsafe { args_pair.as_ref().car });
                let arg_id = Id::fresh(cmp);
                bindings.push((arg_id, anf_arg));
                arg_ids.push(arg_id);

                args = unsafe { args_pair.as_ref().cdr };
            }

            if args == EmptyList::instance(cmp.mt).into() {
                Let(bindings, Box::new(Call(callee_id, arg_ids, HashSet::new())), false)
            } else {
                todo!()
            }
        }

        let params = vec![Id::fresh(self)];
        let body = analyze_expr(self, &Rc::new(Env::Empty), expr);
        r#Fn(LiveVars::new(), params, Box::new(body))        
    }

    fn liveness(expr: &mut anf::Expr) {
        use anf::Expr::{self, *};
        use anf::Triv::*;

        fn live_ins(expr: &mut Expr, mut live_outs: LiveVars) -> LiveVars {
            match expr {
                &mut Let(ref mut bindings, ref mut body, _) => {
                    live_outs = live_ins(body, live_outs);

                    for (id, val) in bindings.iter_mut().rev() {
                        live_outs.remove(id);
                        live_outs = live_ins(val, live_outs);
                    }
                },

                &mut If(ref mut cond, ref mut conseq, ref mut alt, ref mut if_live_outs) => {
                    if_live_outs.extend(live_outs.iter());

                    let conseq_outs = live_outs.clone();
                    let alt_ins = live_ins(alt, live_outs);
                    let mut conseq_ins = live_ins(conseq, conseq_outs);

                    conseq_ins.extend(alt_ins);
                    live_outs = live_ins(cond, conseq_ins);
                },

                &mut r#Fn(ref mut fvs, ref params, ref mut body) => {
                    let mut free_vars = {
                        let mut live_outs = LiveVars::new();
                        live_outs.insert(params[0]); // "self" closure should always be live
                        live_ins(body, live_outs)
                    };

                    for param in params {
                        free_vars.remove(param);
                    }

                    fvs.extend(free_vars.iter());

                    live_outs.extend(free_vars);
                },

                &mut Call(callee, ref args, ref mut call_live_outs) => {
                    call_live_outs.extend(live_outs.iter());

                    for &arg in args.iter().rev() {
                        live_outs.insert(arg);
                    }

                    live_outs.insert(callee);
                },

                &mut Triv(Use(id)) => { live_outs.insert(id); },

                &mut Triv(Const(_)) => ()
            }

            live_outs
        }

        live_ins(expr, LiveVars::new());
    }

    fn to_cfg(&self, expr: &anf::Expr) -> cfg::Fn {
        use anf::Expr::*;
        use anf::Triv::*;

        enum Loc {
            Reg(usize),
            Clover(usize)
        }

        #[derive(Clone)]
        struct Env {
            clovers: Rc<HashMap<Id, usize>>,
            reg_ids: Vec<Option<Id>>,
            id_regs: HashMap<Id, usize>
        }

        impl Env {
            fn new(clover_ids: &[Id], param_ids: &[Id]) -> Self {
                let mut clovers = HashMap::new();
                for (i, &id) in clover_ids.iter().enumerate() {
                    clovers.insert(id, i);
                }

                let mut reg_ids = Vec::with_capacity(param_ids.len());
                let mut id_regs = HashMap::new();
                for (i, &id) in param_ids.iter().enumerate() {
                    reg_ids.push(Some(id));
                    id_regs.insert(id, i);
                }

                Self {
                    clovers: Rc::new(clovers),
                    reg_ids,
                    id_regs
                }
            }

            fn push(&mut self) {
                self.reg_ids.push(None);
            }

            fn name_top(&mut self, id: Id) {
                let reg = self.reg_ids.len() - 1;

                self.reg_ids[reg] = Some(id);
                self.id_regs.insert(id, reg);
            }

            fn popnnt(&mut self, n: usize) {
                let last_index = self.reg_ids.len() - 1;
                for oid in &self.reg_ids[(last_index - n)..last_index] {
                    if let Some(id) = oid {
                        self.id_regs.remove(id);
                    }
                }

                self.reg_ids.truncate(self.reg_ids.len() - n);
            }

            fn pop(&mut self) {
                if let Some(id) = self.reg_ids.pop().flatten() {
                    self.id_regs.remove(&id);
                }
            }

            fn prune(&mut self, prunes: &[bool]) {
                let mut prune_count = 0;
                for (reg, &prune) in prunes.iter().enumerate() {
                    if let Some(id) = self.reg_ids[reg] {
                        if prune {
                            self.id_regs.remove(&id);
                            prune_count += 1;
                        } else {
                            *self.id_regs.get_mut(&id).unwrap() -= prune_count;
                        }
                    }
                }

                for (reg, &prune) in prunes.iter().enumerate().rev() {
                    if prune {
                        self.reg_ids.remove(reg);
                    }
                }
            }

            fn closure(&mut self, nclovers: usize) {
                self.reg_ids.truncate(self.reg_ids.len() - nclovers);
            }

            fn call(&mut self, prunes: &[bool]) {
                self.prune(prunes);
                self.reg_ids.push(None);
            }

            fn get(&self, id: Id) -> Loc {
                self.id_regs.get(&id).map(|&reg| Loc::Reg(reg))
                    .or_else(|| self.clovers.get(&id).map(|&i| Loc::Clover(i)))
                    .unwrap()
            }

            // OPTIMIZE: No need to encode (and eventually decode) callee and args pruning:
            fn prune_mask(&self, live_outs: &HashSet<Id>) -> Vec<bool> {
                let mut mask = vec![true; self.reg_ids.len()];

                for id in live_outs {
                    if let Some(&reg) = self.id_regs.get(id) {
                        mask[reg] = false;
                    }
                }

                mask
            }

            fn join_prune_masks(l: &Self, r: &Self, live_outs: &HashSet<Id>) -> (Vec<bool>, Vec<bool>) {
                let mut lmask = Vec::new();
                let mut rmask = Vec::new();

                let mut loids = l.reg_ids.iter();
                let mut roids = r.reg_ids.iter();
                loop {
                    match loids.next() {
                        Some(Some(lid)) if live_outs.contains(lid) =>
                            loop {
                                match roids.next() {
                                    Some(Some(rid)) if rid == lid => {
                                        lmask.push(false);
                                        rmask.push(false);
                                        break;
                                    },
                                    Some(_) => rmask.push(true),
                                    None => unreachable!()
                                }
                            },

                        Some(_) =>
                            match roids.next() {
                                Some(Some(rid)) if live_outs.contains(rid) => {
                                    lmask.push(true);

                                    loop {
                                        match loids.next() {
                                            Some(Some(lid)) if lid == rid => {
                                                lmask.push(false);
                                                rmask.push(false);
                                                break;
                                            },
                                            Some(_) => lmask.push(true),
                                            None => unreachable!()
                                        }
                                    }
                                },

                                Some(_) => {
                                    lmask.push(false);
                                    rmask.push(false);
                                }

                                None => {
                                    lmask.push(false);
                                    break;
                                }
                            },

                        None => break
                    }
                }

                for _ in loids { lmask.push(false); }
                for _ in roids { rmask.push(false); }

                // Must not prune top, it is the `if` result:
                if let Some(top) = lmask.last_mut() { *top = false; }
                if let Some(top) = rmask.last_mut() { *top = false; }

                (lmask, rmask)
            }
        }

        #[derive(Clone, Copy)]
        enum Cont {
            Next,
            Label(cfg::Label),
            Ret
        }

        fn goto(f: &mut cfg::Fn, current: cfg::Label, cont: Cont) {
            match cont {
                Cont::Next => (),

                Cont::Label(dest) => f.blocks[current].push(cfg::Instr::Goto(dest)),

                Cont::Ret => f.blocks[current].push(cfg::Instr::Ret)
            }
        }

        fn emit_use(env: &mut Env, f: &mut cfg::Fn, current: cfg::Label, r#use: Id) {
            match env.get(r#use) {
                Loc::Reg(reg) => {
                    f.blocks[current].push(cfg::Instr::Local(reg));
                    env.push();
                },

                Loc::Clover(i) => {
                    f.blocks[current].push(cfg::Instr::Clover(i));
                    env.push();
                }
            }
        }

        #[must_use]
        fn emit_expr(cmp: &Compiler, env: &mut Env, f: &mut cfg::Fn, mut current: cfg::Label, cont: Cont,
            expr: &anf::Expr) -> cfg::Label
        {
            match *expr {
                Let(ref bindings, ref body, popnnt) => {
                    for &(id, ref val) in bindings {
                        current = emit_expr(cmp, env, f, current, Cont::Next, val);
                        env.name_top(id);
                    }

                    current = emit_expr(cmp, env, f, current, cont, &**body);

                    let nbs = bindings.len();
                    if popnnt {
                        if let Cont::Ret = cont {
                            /* ret/tailcall will take care of popping */
                        } else {
                            let nbs = bindings.len();
                            if nbs > 0 {
                                f.blocks[current].push(cfg::Instr::PopNNT(nbs));
                                env.popnnt(nbs);
                            }
                        }
                    }

                    current
                },

                If(ref cond, ref conseq, ref alt, ref live_outs) => {
                    let mut conseq_label = f.create_block();
                    let mut alt_label = f.create_block();
                    let join = if let Cont::Next = cont { Cont::Label(f.create_block()) } else { cont };

                    current = emit_expr(cmp, env, f, current, Cont::Next, cond);
                    f.blocks[current].push(cfg::Instr::If(conseq_label, alt_label));
                    env.pop();

                    let mut alt_env = env.clone();

                    conseq_label = emit_expr(cmp, env, f, conseq_label, join, conseq);

                    alt_label = emit_expr(cmp, &mut alt_env, f, alt_label, join, alt);

                    if let Cont::Ret = cont {
                    } else {
                        let (conseq_mask, alt_mask) = Env::join_prune_masks(env, &alt_env, live_outs);
                        env.prune(&conseq_mask);
                        f.insert_prune(conseq_label, conseq_mask);
                        f.insert_prune(alt_label, alt_mask);
                    }

                    if let Cont::Label(join_label) = join { join_label } else { current }
                },

                Fn(ref fvs, ref params, ref body) => {
                    let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();

                    let code = emit_fn(cmp, &fvs, params, body);
                    f.blocks[current].push(cfg::Instr::Code(code));
                    env.push();

                    for &fv in fvs.iter() {
                        emit_use(env, f, current, fv);
                        env.push();
                    }

                    f.blocks[current].push(cfg::Instr::Closure(fvs.len()));
                    env.closure(fvs.len());

                    goto(f, current, cont);

                    current
                },

                Call(_, ref args, ref live_outs) => {
                    // Due to `anf::Expr` construction in `analyze`, code for callee and args already emitted.

                    if let Cont::Ret = cont {
                        f.blocks[current].push(cfg::Instr::TailCall(args.len()));
                    } else {
                        let prunes = env.prune_mask(live_outs);
                        env.call(&prunes);
                        f.blocks[current].push(cfg::Instr::Call(args.len(), prunes));

                        goto(f, current, cont);
                    }

                    current
                },

                Triv(Use(id)) => {
                    emit_use(env, f, current, id);

                    goto(f, current, cont);

                    current
                },

                Triv(Const(ref c)) => {
                    f.blocks[current].push(cfg::Instr::Const(c.clone()));
                    env.push();

                    goto(f, current, cont);

                    current
                }
            }
        }

        fn emit_fn(cmp: &Compiler, clovers: &[Id], params: &[Id], body: &anf::Expr) -> cfg::Fn {
            let mut f = cfg::Fn::new(params.len());
            let current = f.create_block();
            let _ = emit_expr(cmp, &mut Env::new(clovers, params), &mut f, current, Cont::Ret, body);
            f
        }

        if let Fn(ref fvs, ref params, ref body) = expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();
            emit_fn(self, &fvs, params, body)
        } else {
            todo!()
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
