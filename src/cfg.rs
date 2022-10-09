use std::fmt;
use std::rc::Rc;
use std::collections::hash_map::HashMap;

use crate::oref::{WithinMt, DisplayWithin};
use crate::handle::Handle;
use crate::mutator::Mutator;
use crate::anf;

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

impl From<&anf::Expr> for Fn {
   fn from(expr: &anf::Expr) -> Self {
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
            Label(Label),
            Ret
        }

        fn goto(f: &mut Fn, current: Label, cont: Cont) {
            match cont {
                Cont::Next => (),

                Cont::Label(dest) => f.blocks[current].push(Instr::Goto(dest)),

                Cont::Ret => f.blocks[current].push(Instr::Ret)
            }
        }

        fn emit_use(env: &mut Env, f: &mut Fn, current: Label, r#use: anf::Id) {
            match env.get(r#use) {
                Loc::Reg(reg) => {
                    f.blocks[current].push(Instr::Local(reg));
                    env.push();
                },

                Loc::Clover(i) => {
                    f.blocks[current].push(Instr::Clover(i));
                    env.push();
                }
            }
        }

        #[must_use]
        fn emit_expr(env: &mut Env, f: &mut Fn, mut current: Label, cont: Cont, expr: &anf::Expr) -> Label {
            match *expr {
                anf::Expr::Let(ref bindings, ref body, popnnt) => {
                    for &(id, ref val) in bindings {
                        current = emit_expr(env, f, current, Cont::Next, val);
                        env.name_top(id);
                    }

                    current = emit_expr(env, f, current, cont, &**body);

                    if popnnt {
                        if let Cont::Ret = cont {
                            /* ret/tailcall will take care of popping */
                        } else {
                            let nbs = bindings.len();
                            if nbs > 0 {
                                f.blocks[current].push(Instr::PopNNT(nbs));
                                env.popnnt(nbs);
                            }
                        }
                    }

                    current
                },

                anf::Expr::If(ref cond, ref conseq, ref alt, ref live_outs) => {
                    let mut conseq_label = f.create_block();
                    let mut alt_label = f.create_block();
                    let join = if let Cont::Next = cont { Cont::Label(f.create_block()) } else { cont };

                    current = emit_expr(env, f, current, Cont::Next, cond);
                    f.blocks[current].push(Instr::If(conseq_label, alt_label));
                    env.pop();

                    let mut alt_env = env.clone();

                    conseq_label = emit_expr(env, f, conseq_label, join, conseq);

                    alt_label = emit_expr(&mut alt_env, f, alt_label, join, alt);

                    if let Cont::Ret = cont {
                    } else {
                        let (conseq_mask, alt_mask) = Env::join_prune_masks(env, &alt_env, live_outs);
                        env.prune(&conseq_mask);
                        f.insert_prune(conseq_label, conseq_mask);
                        f.insert_prune(alt_label, alt_mask);
                    }

                    if let Cont::Label(join_label) = join { join_label } else { current }
                },

                anf::Expr::Fn(ref fvs, ref params, ref body) => {
                    let fvs = fvs.iter().map(|&id| id).collect::<Vec<anf::Id>>();

                    let code = emit_fn(&fvs, params, body);
                    f.blocks[current].push(Instr::Code(code));
                    env.push();

                    for &fv in fvs.iter() {
                        emit_use(env, f, current, fv);
                        env.push();
                    }

                    f.blocks[current].push(Instr::Closure(fvs.len()));
                    env.closure(fvs.len());

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Call(_, ref args, ref live_outs) => {
                    // Due to `anf::Expr` construction in `analyze`, code for callee and args already emitted.

                    if let Cont::Ret = cont {
                        f.blocks[current].push(Instr::TailCall(args.len()));
                    } else {
                        let prunes = env.prune_mask(live_outs);
                        env.call(&prunes);
                        f.blocks[current].push(Instr::Call(args.len(), prunes));

                        goto(f, current, cont);
                    }

                    current
                },

                anf::Expr::Triv(anf::Triv::Use(id)) => {
                    emit_use(env, f, current, id);

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Triv(anf::Triv::Const(ref c)) => {
                    f.blocks[current].push(Instr::Const(c.clone()));
                    env.push();

                    goto(f, current, cont);

                    current
                }
            }
        }

        fn emit_fn(clovers: &[anf::Id], params: &[anf::Id], body: &anf::Expr) -> Fn {
            let mut f = Fn::new(params.len());
            let current = f.create_block();
            let _ = emit_expr(&mut Env::new(clovers, params), &mut f, current, Cont::Ret, body);
            f
        }

        if let anf::Expr::Fn(ref fvs, ref params, ref body) = expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<anf::Id>>();
            emit_fn(&fvs, params, body)
        } else {
            todo!()
        }
    }
}
