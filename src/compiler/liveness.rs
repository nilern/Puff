use crate::compiler::anf;

pub fn liveness(expr: &mut anf::Expr) {
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
