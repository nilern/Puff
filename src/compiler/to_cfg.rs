use std::rc::Rc;
use std::collections::hash_map::HashMap;

use crate::oref::ORef;
use crate::vector::Vector;
use crate::bool::Bool;
use crate::handle::Handle;
use crate::compiler::cfg::{Fn, Instr, Label};
use crate::compiler::anf;
use crate::compiler::{Compiler, Id};

struct CfgBuilder {
    f: Fn,
    current: Label
}

impl CfgBuilder {
    fn push(&mut self, instr: Instr) { self.f.block_mut(self.current).push(instr); }
}

enum Loc {
    Reg(usize),
    Clover(usize)
}

#[derive(Clone)]
struct Env {
    clovers: Rc<HashMap<Id, usize>>,
    reg_ids: Vec<Option<Id>>,
    id_regs: HashMap<Id, usize>,
    max_regs: usize
}

impl Env {
    fn new(clover_ids: &[Id], param_ids: &[Id]) -> Self {
        let mut clovers: HashMap<Id, usize> = clover_ids.iter()
            .enumerate()
            .map(|(i, &id)| (id, i))
            .collect();

        let mut reg_ids = Vec::with_capacity(param_ids.len());
        let mut id_regs = HashMap::new();
        for (i, &id) in param_ids.iter().enumerate() {
            reg_ids.push(Some(id));
            id_regs.insert(id, i);
        }

        Self {
            clovers: Rc::new(clovers),
            reg_ids,
            id_regs,
            max_regs: param_ids.len()
        }
    }

    fn push(&mut self) {
        self.reg_ids.push(None);
        self.max_regs = self.max_regs.max(self.reg_ids.len());
    }

    fn name_top(&mut self, id: Id) {
        let reg = self.reg_ids.len() - 1;

        self.reg_ids[reg] = Some(id);
        self.id_regs.insert(id, reg);
    }

    fn pop(&mut self) {
        if let Some(id) = self.reg_ids.pop().unwrap() {
            self.id_regs.remove(&id);
        }
    }

    fn popn(&mut self, n: usize) {
        for oid in &self.reg_ids[(self.reg_ids.len() - n)..] {
            if let Some(id) = oid {
                self.id_regs.remove(id);
            }
        }

        self.reg_ids.truncate(self.reg_ids.len() - n);
    }

    fn popnnt(&mut self, n: usize) {
        let last_index = self.reg_ids.len() - 1;
        for oid in &self.reg_ids[(last_index - n)..last_index] {
            if let Some(id) = oid {
                self.id_regs.remove(id);
            }
        }

        let top = *self.reg_ids.last().unwrap();
        self.reg_ids.truncate(self.reg_ids.len() - n);
        *self.reg_ids.last_mut().unwrap() = top;
        if let Some(id) = top {
            self.id_regs.insert(id, self.reg_ids.len() - 1);
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

    fn call(&mut self, cargc: usize, prunes: &[bool]) {
        self.prune(prunes);
        self.popn(cargc);
        self.reg_ids.push(None);
    }

    fn get(&self, id: Id) -> Loc {
        self.id_regs.get(&id).map(|&reg| Loc::Reg(reg))
            .or_else(|| self.clovers.get(&id).map(|&i| Loc::Clover(i)))
            .unwrap_or_else(|| panic!("Unbound {:?}", id))
    }

    fn prune_mask(&self, cargc: usize, live_outs: &anf::LiveVars) -> Vec<bool> {
        let mut mask = vec![true; self.reg_ids.len() - cargc];

        for id in live_outs {
            if let Some(&reg) = self.id_regs.get(id) {
                let prune = mask.get_mut(reg).unwrap_or_else(|| {
                    panic!("Live variable {:?} in callee/arg register {}", id, reg)
                });
                *prune = false;
            }
        }

        mask
    }

    fn join_prune_masks(l: &Self, r: &Self, live_outs: &anf::LiveVars) -> (Vec<bool>, Vec<bool>) {
        let mut lmask = Vec::new();
        let mut rmask = Vec::new();

        // Must not prune top, it is the `if` result:
        let mut loids = l.reg_ids[..(l.reg_ids.len() - 1)].iter().rev();
        let mut roids = r.reg_ids[..(r.reg_ids.len() - 1)].iter().rev();
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

        lmask.reverse();
        rmask.reverse();

        (lmask, rmask)
    }
}

#[derive(Clone, Copy)]
enum Cont {
    Next,
    Label(Label),
    Ret
}

impl Cont {
    fn goto(self, builder: &mut CfgBuilder) {
        match self {
            Cont::Next => (),
            Cont::Label(dest) => builder.push(Instr::Goto(dest)),
            Cont::Ret => builder.push(Instr::Ret)
        }
    }
}

fn emit_use(env: &mut Env, builder: &mut CfgBuilder, r#use: Id) {
    match env.get(r#use) {
        Loc::Reg(reg) => {
            builder.push(Instr::Local(reg));
            env.push();
        },

        Loc::Clover(i) => {
            builder.push(Instr::Clover(i));
            env.push();
        }
    }
}

fn emit_expr(cmp: &mut Compiler, env: &mut Env, builder: &mut CfgBuilder, cont: Cont, expr: &anf::Expr) {
    match *expr {
        anf::Expr::Define(ref definiend, ref val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next, val_expr);
            builder.push(Instr::Define(definiend.clone()));
            env.pop();
            env.push();

            cont.goto(builder);
        },
        anf::Expr::GlobalSet(ref definiend, ref val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next, val_expr);
            builder.push(Instr::GlobalSet(definiend.clone()));
            env.pop();
            env.push();

            cont.goto(builder);
        },

        anf::Expr::Begin(ref stmts) => {
            for stmt in &stmts[0..(stmts.len() - 1)] {
                emit_expr(cmp, env, builder, Cont::Next, stmt);
            }

            emit_expr(cmp, env, builder, cont, stmts.last().unwrap());

            if let Cont::Ret = cont {
                /* ret/tailcall will take care of popping */
            } else {
                let pop_count = stmts.len() - 1;
                if pop_count > 0 {
                    builder.push(Instr::PopNNT(pop_count));
                    env.popnnt(pop_count);
                }
            }
        }

        anf::Expr::Let(ref bindings, ref body) => {
            for &(id, ref val) in bindings {
                emit_expr(cmp, env, builder, Cont::Next, val);
                env.name_top(id);
            }

            emit_expr(cmp, env, builder, cont, &**body);

            if let Cont::Ret = cont {
                /* ret/tailcall will take care of popping */
            } else {
                let nbs = bindings.len();
                if nbs > 0 {
                    builder.push(Instr::PopNNT(nbs));
                    env.popnnt(nbs);
                }
            }
        },

        anf::Expr::If(ref cond, ref conseq, ref alt, ref live_outs) => {
            let mut conseq_label = builder.f.create_block();
            let mut alt_label = builder.f.create_block();
            let join = if let Cont::Next = cont { Cont::Label(builder.f.create_block()) } else { cont };

            emit_expr(cmp, env, builder, Cont::Next, cond);
            builder.push(Instr::If(conseq_label, alt_label));
            env.pop();

            let mut alt_env = env.clone();

            builder.current = conseq_label;
            emit_expr(cmp, env, builder, join, conseq);

            builder.current = alt_label;
            emit_expr(cmp, &mut alt_env, builder, join, alt);

            if let Cont::Ret = cont {
            } else {
                let (conseq_mask, alt_mask) = Env::join_prune_masks(env, &alt_env, live_outs);
                env.prune(&conseq_mask);
                if conseq_mask.iter().any(|&prune| prune) { builder.f.insert_prune(conseq_label, conseq_mask); }
                if alt_mask.iter().any(|&prune| prune) { builder.f.insert_prune(alt_label, alt_mask); }
            }
            env.max_regs = env.max_regs.max(alt_env.max_regs);

            if let Cont::Label(join_label) = join {
                builder.current = join_label;
            }
        },

        anf::Expr::UninitializedBox => {
            builder.push(Instr::UninitializedBox);
            env.push();

            cont.goto(builder);
        },

        anf::Expr::Box(ref val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next, val_expr);
            builder.push(Instr::Box);
            env.pop();
            env.push();

            cont.goto(builder);
        },

        anf::Expr::BoxSet(r#box, ref val_expr) => {
            emit_use(env, builder, r#box);
            emit_expr(cmp, env, builder, Cont::Next, val_expr);
            builder.push(Instr::BoxSet);
            env.popn(2);
            env.push();

            cont.goto(builder);
        },

        anf::Expr::CheckedBoxSet {guard, r#box, ref val_expr} => {
            emit_use(env, builder, guard);
            emit_use(env, builder, r#box);
            emit_expr(cmp, env, builder, Cont::Next, val_expr);
            builder.push(Instr::CheckedBoxSet);
            env.popn(3);
            env.push();

            cont.goto(builder);
        },

        anf::Expr::BoxGet(r#box) => {
            emit_use(env, builder, r#box);
            builder.push(Instr::BoxGet);
            env.pop();
            env.push();

            cont.goto(builder);
        },

        anf::Expr::CheckedBoxGet {guard, r#box} => {
            emit_use(env, builder, guard);
            emit_use(env, builder, r#box);
            builder.push(Instr::CheckedBoxGet);
            env.popn(2);
            env.push();

            cont.goto(builder);
        },

        anf::Expr::Fn(ref fvs, ref params, varargs, ref body) => {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();

            for &fv in fvs.iter() {
                emit_use(env, builder, fv);
            }

            let code = emit_fn(cmp, &fvs, params, varargs, body);
            builder.push(Instr::Fn(code, fvs.len()));
            env.popn(fvs.len());
            env.push();

            cont.goto(builder);
        },

        anf::Expr::Call(ref cargs, ref live_outs) => {
            for &(id, ref val) in cargs {
                emit_expr(cmp, env, builder, Cont::Next, val);
                env.name_top(id);
            }

            if let Cont::Ret = cont {
                builder.push(Instr::TailCall(cargs.len()));
            } else {
                let prunes = env.prune_mask(cargs.len(), live_outs);
                env.call(cargs.len(), &prunes);
                builder.push(Instr::Call(cargs.len(), prunes));

                cont.goto(builder);
            }
        },

        anf::Expr::Global(ref name) => {
            builder.push(Instr::Global(name.clone()));
            env.push();

            cont.goto(builder);
        },

        anf::Expr::CheckedUse {guard, id} => {
            emit_use(env, builder, guard);
            builder.push(Instr::CheckUse);
            env.pop();
            emit_use(env, builder, id);

            cont.goto(builder);
        },

        anf::Expr::Triv(anf::Triv::Use(id)) => {
            emit_use(env, builder, id);

            cont.goto(builder);
        },

        anf::Expr::Triv(anf::Triv::Const(ref c)) => {
            builder.push(Instr::Const(c.clone()));
            env.push();

            cont.goto(builder);
        },

        anf::Expr::Letrec(..) | anf::Expr::Set(..) => unreachable!()
    }
}

fn emit_fn(cmp: &mut Compiler, clovers: &[Id], params: &[Id], varargs: bool, body: &anf::Expr) -> Fn {
    let mut env = Env::new(clovers, params);
    let mut f = {
        // OPTIMIZE: temp Vec:
        let clover_names: Vec<Handle> = clovers.iter()
            .map(|id| id.name(cmp).map(Handle::from)
                .unwrap_or_else(|| cmp.mt.root(Bool::instance(cmp.mt, false).into())))
            .collect();
        let clover_names = {
            let clover_names = Vector::<ORef>::from_handles(cmp.mt, &clover_names);
            cmp.mt.root_t(clover_names)
        };
        Fn::new(params.len() - varargs as usize, varargs, 0, clover_names)
    };
    let current = f.create_block();
    let mut builder = CfgBuilder {f, current};

    emit_expr(cmp, &mut env, &mut builder, Cont::Ret, body);

    let mut f = builder.f;
    f.max_regs = env.max_regs;
    f
}

impl Fn {
   pub fn from_anf(cmp: &mut Compiler, expr: &anf::Expr) -> Self {
        if let anf::Expr::Fn(ref fvs, ref params, varargs, ref body) = expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();
            emit_fn(cmp, &fvs, params, *varargs, body)
        } else {
            todo!()
        }
    }
}
