use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::rc::Rc;

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

    let mut anf = analyze(&mut cmp, expr);
    let cfg = cmp.to_cfg(&anf);
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

    fn to_cfg(&self, expr: &anf::Expr) -> cfg::Fn {
        use anf::Expr::*;
        use anf::Triv::*;

        enum Loc {
            Reg(usize),
            Clover(usize)
        }

        #[derive(Clone)]
        struct Env {
            clovers: Rc<HashMap<anf::Id, usize>>,
            reg_ids: Vec<Option<anf::Id>>,
            id_regs: HashMap<anf::Id, usize>
        }

        impl Env {
            fn new(clover_ids: &[anf::Id], param_ids: &[anf::Id]) -> Self {
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

            fn name_top(&mut self, id: anf::Id) {
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

            fn get(&self, id: anf::Id) -> Loc {
                self.id_regs.get(&id).map(|&reg| Loc::Reg(reg))
                    .or_else(|| self.clovers.get(&id).map(|&i| Loc::Clover(i)))
                    .unwrap()
            }

            // OPTIMIZE: No need to encode (and eventually decode) callee and args pruning:
            fn prune_mask(&self, live_outs: &anf::LiveVars) -> Vec<bool> {
                let mut mask = vec![true; self.reg_ids.len()];

                for id in live_outs {
                    if let Some(&reg) = self.id_regs.get(id) {
                        mask[reg] = false;
                    }
                }

                mask
            }

            fn join_prune_masks(l: &Self, r: &Self, live_outs: &anf::LiveVars) -> (Vec<bool>, Vec<bool>) {
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

        fn emit_use(env: &mut Env, f: &mut cfg::Fn, current: cfg::Label, r#use: anf::Id) {
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
                    let fvs = fvs.iter().map(|&id| id).collect::<Vec<anf::Id>>();

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

        fn emit_fn(cmp: &Compiler, clovers: &[anf::Id], params: &[anf::Id], body: &anf::Expr) -> cfg::Fn {
            let mut f = cfg::Fn::new(params.len());
            let current = f.create_block();
            let _ = emit_expr(cmp, &mut Env::new(clovers, params), &mut f, current, Cont::Ret, body);
            f
        }

        if let Fn(ref fvs, ref params, ref body) = expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<anf::Id>>();
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
