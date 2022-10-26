use std::rc::Rc;
use std::collections::hash_map::HashMap;

use crate::cfg::{Fn, Instr, Label};
use crate::anf;
use crate::compiler::Id;

impl From<&anf::Expr> for Fn {
   fn from(expr: &anf::Expr) -> Self {
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
                if let Some(id) = self.reg_ids.pop().flatten() {
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

                self.reg_ids.truncate(self.reg_ids.len() - n);
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

            fn call(&mut self, argc: usize, prunes: &[bool]) {
                self.prune(prunes);
                self.popn(argc + 1);
                self.reg_ids.push(None);
            }

            fn get(&self, id: Id) -> Loc {
                self.id_regs.get(&id).map(|&reg| Loc::Reg(reg))
                    .or_else(|| self.clovers.get(&id).map(|&i| Loc::Clover(i)))
                    .unwrap_or_else(|| panic!("Unbound {:?}", id))
            }

            fn prune_mask(&self, argc: usize, live_outs: &anf::LiveVars) -> Vec<bool> {
                let mut mask = vec![true; self.reg_ids.len() - argc - 1];

                for id in live_outs {
                    self.id_regs.get(id)
                        .and_then(|&reg| mask.get_mut(reg))
                        .map(|dest| *dest = false);
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

        fn goto(f: &mut Fn, current: Label, cont: Cont) {
            match cont {
                Cont::Next => (),

                Cont::Label(dest) => f.block_mut(current).push(Instr::Goto(dest)),

                Cont::Ret => f.block_mut(current).push(Instr::Ret)
            }
        }

        fn emit_use(env: &mut Env, f: &mut Fn, current: Label, r#use: Id) {
            match env.get(r#use) {
                Loc::Reg(reg) => {
                    f.block_mut(current).push(Instr::Local(reg));
                    env.push();
                },

                Loc::Clover(i) => {
                    f.block_mut(current).push(Instr::Clover(i));
                    env.push();
                }
            }
        }

        #[must_use]
        fn emit_expr(env: &mut Env, f: &mut Fn, mut current: Label, cont: Cont, expr: &anf::Expr) -> Label {
            match *expr {
                anf::Expr::Define(ref definiend, ref val_expr) => {
                    current = emit_expr(env, f, current, Cont::Next, val_expr);
                    f.block_mut(current).push(Instr::Define(definiend.clone()));
                    env.pop();
                    env.push();

                    goto(f, current, cont);

                    current
                },
                anf::Expr::GlobalSet(ref definiend, ref val_expr) => {
                    current = emit_expr(env, f, current, Cont::Next, val_expr);
                    f.block_mut(current).push(Instr::GlobalSet(definiend.clone()));
                    env.pop();
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Begin(ref stmts) => {
                    for stmt in &stmts[0..(stmts.len() - 1)] {
                        current = emit_expr(env, f, current, Cont::Next, stmt);
                    }

                    current = emit_expr(env, f, current, cont, stmts.last().unwrap());

                    if let Cont::Ret = cont {
                        /* ret/tailcall will take care of popping */
                    } else {
                        let pop_count = stmts.len() - 1;
                        if pop_count > 0 {
                            f.block_mut(current).push(Instr::PopNNT(pop_count));
                            env.popnnt(pop_count);
                        }
                    }

                    current
                }

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
                                f.block_mut(current).push(Instr::PopNNT(nbs));
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
                    f.block_mut(current).push(Instr::If(conseq_label, alt_label));
                    env.pop();

                    let mut alt_env = env.clone();

                    conseq_label = emit_expr(env, f, conseq_label, join, conseq);

                    alt_label = emit_expr(&mut alt_env, f, alt_label, join, alt);


                    if let Cont::Ret = cont {
                    } else {
                        let (conseq_mask, alt_mask) = Env::join_prune_masks(env, &alt_env, live_outs);
                        env.prune(&conseq_mask);
                        if conseq_mask.iter().any(|&prune| prune) { f.insert_prune(conseq_label, conseq_mask); }
                        if alt_mask.iter().any(|&prune| prune) { f.insert_prune(alt_label, alt_mask); }
                    }
                    env.max_regs = env.max_regs.max(alt_env.max_regs);

                    if let Cont::Label(join_label) = join { join_label } else { current }
                },

                anf::Expr::Set(..) => unreachable!(),

                anf::Expr::UninitializedBox => {
                    f.block_mut(current).push(Instr::UninitializedBox);
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Box(ref val_expr) => {
                    current = emit_expr(env, f, current, Cont::Next, val_expr);
                    f.block_mut(current).push(Instr::Box);
                    env.pop();
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::BoxSet(r#box, ref val_expr) => {
                    emit_use(env, f, current, r#box);
                    current = emit_expr(env, f, current, Cont::Next, val_expr);
                    f.block_mut(current).push(Instr::BoxSet);
                    env.popn(2);
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::CheckedBoxSet {guard, r#box, ref val_expr} => {
                    emit_use(env, f, current, guard);
                    emit_use(env, f, current, r#box);
                    current = emit_expr(env, f, current, Cont::Next, val_expr);
                    f.block_mut(current).push(Instr::CheckedBoxSet);
                    env.popn(3);
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::BoxGet(r#box) => {
                    emit_use(env, f, current, r#box);
                    f.block_mut(current).push(Instr::BoxGet);
                    env.pop();
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::CheckedBoxGet {guard, r#box} => {
                    emit_use(env, f, current, guard);
                    emit_use(env, f, current, r#box);
                    f.block_mut(current).push(Instr::CheckedBoxGet);
                    env.popn(2);
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Fn(ref fvs, ref params, varargs, ref body) => {
                    let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();

                    for &fv in fvs.iter() {
                        emit_use(env, f, current, fv);
                    }

                    let code = emit_fn(&fvs, params, varargs, body);
                    f.block_mut(current).push(Instr::Fn(code, fvs.len()));
                    env.popn(fvs.len());
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Call(_, ref args, ref live_outs) => {
                    // Due to `anf::Expr` construction in `analyze`, code for callee and args already emitted.

                    if let Cont::Ret = cont {
                        f.block_mut(current).push(Instr::TailCall(args.len()));
                    } else {
                        let prunes = env.prune_mask(args.len(), live_outs);
                        env.call(args.len(), &prunes);
                        f.block_mut(current).push(Instr::Call(args.len(), prunes));

                        goto(f, current, cont);
                    }

                    current
                },

                anf::Expr::Global(ref name) => {
                    f.block_mut(current).push(Instr::Global(name.clone()));
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::CheckedUse {guard, id} => {
                    emit_use(env, f, current, guard);
                    f.block_mut(current).push(Instr::CheckUse);
                    env.pop();
                    emit_use(env, f, current, id);

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Triv(anf::Triv::Use(id)) => {
                    emit_use(env, f, current, id);

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Triv(anf::Triv::Const(ref c)) => {
                    f.block_mut(current).push(Instr::Const(c.clone()));
                    env.push();

                    goto(f, current, cont);

                    current
                },

                anf::Expr::Letrec(..) => unreachable!()
            }
        }

        fn emit_fn(clovers: &[Id], params: &[Id], varargs: bool, body: &anf::Expr) -> Fn {
            let mut env = Env::new(clovers, params);
            let mut f = Fn::new(params.len() - varargs as usize, varargs, 0, clovers.len());
            let current = f.create_block();
            let _ = emit_expr(&mut env, &mut f, current, Cont::Ret, body);
            f.max_regs = env.max_regs;
            f
        }

        if let anf::Expr::Fn(ref fvs, ref params, varargs, ref body) = expr {
            let fvs = fvs.iter().map(|&id| id).collect::<Vec<Id>>();
            emit_fn(&fvs, params, *varargs, body)
        } else {
            todo!()
        }
    }
}
