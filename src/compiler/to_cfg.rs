use std::rc::Rc;
use std::collections::hash_map::HashMap;

use crate::oref::ORef;
use crate::vector::Vector;
use crate::bool::Bool;
use crate::handle::{Handle, Root, root};
use crate::compiler::cfg::{Fn, Instr, Label, PosInstr};
use crate::compiler::anf::{self, PosExpr};
use crate::compiler::{Compiler, Id};

struct CfgBuilder {
    f: Fn,
    current: Label
}

impl CfgBuilder {
    fn push(&mut self, instr: Instr, pos: Handle) { self.f.block_mut(self.current).push(PosInstr {instr, pos}); }
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
        let clovers: HashMap<Id, usize> = clover_ids.iter()
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
    Next {ignore_values: bool},
    Label {label: Label, ignore_values: bool},
    Ret
}

impl Cont {
    fn goto(self, env: &mut Env, builder: &mut CfgBuilder, pos: Handle) {
        match self {
            Cont::Next {ignore_values} =>
                if ignore_values {
                    builder.push(Instr::Pop, pos);
                    env.pop();
                },

            Cont::Label {label: dest, ignore_values} => {
                if ignore_values {
                    builder.push(Instr::Pop, pos.clone());
                    env.pop();
                }
                builder.push(Instr::Goto(dest), pos);
            },

            Cont::Ret => builder.push(Instr::Ret, pos)
        }
    }
}

fn emit_use(env: &mut Env, builder: &mut CfgBuilder, r#use: Id, pos: Handle) {
    match env.get(r#use) {
        Loc::Reg(reg) => {
            builder.push(Instr::Local(reg), pos);
            env.push();
        },

        Loc::Clover(i) => {
            builder.push(Instr::Clover(i), pos);
            env.push();
        }
    }
}

fn emit_expr(cmp: &mut Compiler, env: &mut Env, builder: &mut CfgBuilder, cont: Cont, expr: PosExpr) {
    let PosExpr {pos, expr} = expr;

    match expr {
        anf::Expr::Define(definiend, val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *val_expr);
            builder.push(Instr::Define(definiend), pos.clone());
            env.pop();
            env.push();

            cont.goto(env, builder, pos);
        },
        anf::Expr::GlobalSet(definiend, val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *val_expr);
            builder.push(Instr::GlobalSet(definiend), pos.clone());
            env.pop();
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::Begin(mut stmts) => {
            let body = stmts.pop().unwrap();

            for stmt in stmts {
                emit_expr(cmp, env, builder, Cont::Next {ignore_values: true}, stmt);
            }

            emit_expr(cmp, env, builder, cont, body);
        }

        anf::Expr::Let(bindings, body) => {
            let bindingc = bindings.len();
            let body_pos = body.pos.clone();

            for (id, val) in bindings {
                emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, val);
                env.name_top(id);
            }

            emit_expr(cmp, env, builder, cont, *body);

            if let Cont::Ret = cont {
                /* ret/tailcall will take care of popping */
            } else {
                if bindingc > 0 {
                    builder.push(Instr::PopNNT(bindingc), body_pos);
                    env.popnnt(bindingc);
                }
            }
        },

        anf::Expr::If(cond, conseq, alt, live_outs) => {
            let conseq_label = builder.f.create_block();
            let alt_label = builder.f.create_block();
            let join = if let Cont::Next {ignore_values} = cont {
                Cont::Label {label: builder.f.create_block(), ignore_values}
            } else {
                cont
            };

            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *cond);
            builder.push(Instr::If(conseq_label, alt_label), pos);
            env.pop();

            let mut alt_env = env.clone();

            let conseq_pos = conseq.pos.clone();
            builder.current = conseq_label;
            emit_expr(cmp, env, builder, join, *conseq);

            let alt_pos = alt.pos.clone();
            builder.current = alt_label;
            emit_expr(cmp, &mut alt_env, builder, join, *alt);

            if let Cont::Ret = cont {
            } else {
                let (conseq_mask, alt_mask) = Env::join_prune_masks(env, &alt_env, &live_outs);
                env.prune(&conseq_mask);
                if conseq_mask.iter().any(|&prune| prune) {
                    builder.f.insert_prune(conseq_label, conseq_mask, conseq_pos);
                }
                if alt_mask.iter().any(|&prune| prune) {
                    builder.f.insert_prune(alt_label, alt_mask, alt_pos);
                }
            }
            env.max_regs = env.max_regs.max(alt_env.max_regs);

            if let Cont::Label {label: join_label, ignore_values: _} = join {
                builder.current = join_label;
            }
        },

        anf::Expr::UninitializedBox => {
            builder.push(Instr::UninitializedBox, pos.clone());
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::Box(val_expr) => {
            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *val_expr);
            builder.push(Instr::Box, pos.clone());
            env.pop();
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::BoxSet(r#box, val_expr) => {
            emit_use(env, builder, r#box, pos.clone());
            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *val_expr);
            builder.push(Instr::BoxSet, pos.clone());
            env.popn(2);
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::CheckedBoxSet {guard, r#box, val_expr} => {
            emit_use(env, builder, guard, pos.clone());
            emit_use(env, builder, r#box, pos.clone());
            emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, *val_expr);
            builder.push(Instr::CheckedBoxSet, pos.clone());
            env.popn(3);
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::BoxGet(r#box) => {
            emit_use(env, builder, r#box, pos.clone());
            builder.push(Instr::BoxGet, pos.clone());
            env.pop();
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::CheckedBoxGet {guard, r#box} => {
            emit_use(env, builder, guard, pos.clone());
            emit_use(env, builder, r#box, pos.clone());
            builder.push(Instr::CheckedBoxGet, pos.clone());
            env.popn(2);
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::Fn(ref fvs, ref params, varargs, body) => {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();

            for &fv in fvs.iter() {
                emit_use(env, builder, fv, pos.clone());
            }

            let code = emit_fn(cmp, &fvs, params, varargs, *body);
            builder.push(Instr::Fn(code, fvs.len()), pos.clone());
            env.popn(fvs.len());
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::Call(cargs, live_outs) => {
            let cargc = cargs.len();

            for (id, val) in cargs {
                emit_expr(cmp, env, builder, Cont::Next {ignore_values: false}, val);
                env.name_top(id);
            }

            match cont {
                Cont::Next {ignore_values} | Cont::Label {label: _, ignore_values} => {
                    let prunes = env.prune_mask(cargc, &live_outs);
                    env.prune(&prunes);
                    env.popn(cargc);
                    builder.push(Instr::Call(cargc, prunes), pos.clone());

                    if !ignore_values {
                        builder.push(Instr::CheckOneReturnValue, pos.clone());
                        env.push();

                        cont.goto(env, builder, pos);
                    } else {
                        builder.push(Instr::IgnoreReturnValues, pos.clone());

                        match cont {
                            Cont::Next {ignore_values: _} => (),
                            Cont::Label {label, ignore_values: _} => builder.push(Instr::Goto(label), pos),
                            Cont::Ret => unreachable!()
                        }
                    }
                },

                Cont::Ret => builder.push(Instr::TailCall(cargc), pos)
            }
        },

        anf::Expr::Global(name) => {
            builder.push(Instr::Global(name), pos.clone());
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::CheckedUse {guard, id} => {
            emit_use(env, builder, guard, pos.clone());
            builder.push(Instr::CheckUse, pos.clone());
            env.pop();
            emit_use(env, builder, id, pos.clone());

            cont.goto(env, builder, pos);
        },

        anf::Expr::Triv(anf::Triv::Use(id)) => {
            emit_use(env, builder, id, pos.clone());

            cont.goto(env, builder, pos);
        },

        anf::Expr::Triv(anf::Triv::Const(c)) => {
            builder.push(Instr::Const(c), pos.clone());
            env.push();

            cont.goto(env, builder, pos);
        },

        anf::Expr::Letrec(..) | anf::Expr::Set(..) => unreachable!()
    }
}

fn emit_fn(cmp: &mut Compiler, clovers: &[Id], params: &[Id], varargs: bool, body: PosExpr) -> Fn {
    let mut env = Env::new(clovers, params);
    let mut f = {
        // OPTIMIZE: temp Vec:
        let clover_names: Vec<Handle> = clovers.iter()
            .map(|id| id.name(cmp).map(Handle::from)
                .unwrap_or_else(|| root!(&mut cmp.mt, Bool::instance(cmp.mt, false)).into()))
            .collect();
        let clover_names = root!(&mut cmp.mt, Vector::<ORef>::from_handles(cmp.mt, &clover_names));
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
   pub fn from_anf(cmp: &mut Compiler, expr: PosExpr) -> Self {
        if let anf::Expr::Fn(fvs, params, varargs, body) = expr.expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();
            emit_fn(cmp, &fvs, &params, varargs, *body)
        } else {
            todo!()
        }
    }
}
