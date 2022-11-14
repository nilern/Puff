use crate::compiler::Id;
use crate::compiler::anf::{self, PosExpr};

pub fn liveness(expr: &mut anf::PosExpr) {
    use anf::Expr::*;
    use anf::Triv::*;

    fn fn_clause_free_vars(params: &[Id], body: &mut PosExpr) -> anf::LiveVars {
        let mut free_vars = {
            let mut live_outs = anf::LiveVars::new();
            live_outs.insert(params[0]); // "self" closure should always be live
            live_ins(body, live_outs)
        };

        for param in params {
            free_vars.remove(param);
        }

        free_vars
    }

    fn live_ins(expr: &mut PosExpr, mut live_outs: anf::LiveVars) -> anf::LiveVars {
        match expr.expr {
            Define(_, ref mut val_expr) => live_ins(val_expr, live_outs),
            GlobalSet(_, ref mut val_expr) => live_ins(val_expr, live_outs),

            Begin(ref mut stmts) =>
                stmts.iter_mut().rev()
                    .fold(live_outs, |live_outs, stmt| live_ins(stmt, live_outs)),

            Let(ref mut bindings, ref mut body, ref mut let_live_outs) => {
                *let_live_outs = live_outs.clone();

                let body_live_ins = live_ins(body, live_outs);

                bindings.iter_mut().rev()
                    .fold(body_live_ins, |mut live_outs, (id, val_expr)| {
                        live_outs.remove(id);
                        live_ins(val_expr, live_outs)
                    })
            },

            If(ref mut cond, ref mut conseq, ref mut alt, ref mut if_live_outs) => {
                *if_live_outs = live_outs.clone();

                let conseq_outs = live_outs.clone();
                let alt_ins = live_ins(alt, live_outs);
                let mut conseq_ins = live_ins(conseq, conseq_outs);

                conseq_ins.extend(alt_ins);
                live_ins(cond, conseq_ins)
            },

            Box(ref mut val_expr) => live_ins(val_expr, live_outs),

            UninitializedBox => live_outs,

            BoxSet(r#box, ref mut val_expr) => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins
            },

            CheckedBoxSet {guard, r#box, ref mut val_expr} => {
                let mut val_expr_live_ins = live_ins(val_expr, live_outs);
                val_expr_live_ins.insert(r#box);
                val_expr_live_ins.insert(guard);
                val_expr_live_ins
            },

            BoxGet(r#box) => { live_outs.insert(r#box); live_outs }

            CheckedBoxGet {guard, r#box} => {
                live_outs.insert(r#box);
                live_outs.insert(guard);
                live_outs
            }

            r#Fn(ref mut fvs, ref params, _, ref mut body) => {
                let free_vars = fn_clause_free_vars(params, body);

                *fvs = free_vars.clone();

                live_outs.extend(free_vars);
                live_outs
            },

            CaseFn(ref mut clauses) => {
                for (_, ref mut fvs, ref params, _, ref mut body) in clauses.iter_mut() {
                    let free_vars = fn_clause_free_vars(params, body);

                    *fvs = free_vars.clone();

                    live_outs.extend(free_vars);
                }

                live_outs
            },

            Call(ref mut cargs, ref mut call_live_outs) => {
                *call_live_outs = live_outs.clone();

                for (id, _) in cargs.iter() {
                    live_outs.insert(*id);
                }

                cargs.iter_mut().rev()
                    .fold(live_outs, |mut live_outs, (id, val_expr)| {
                        live_outs.remove(id);
                        live_ins(val_expr, live_outs)
                    })
            },

            CallWithValues((pid, ref mut producer), (_, ref mut consumer), ref mut call_live_outs) => {
                *call_live_outs = live_outs.clone();

                live_outs.insert(pid);
                live_outs = live_ins(consumer, live_outs);
                live_outs.remove(&pid);
                live_ins(producer, live_outs)
            },

            Global(_) => live_outs,

            CheckedUse {guard, id} => {
                live_outs.insert(id);
                live_outs.insert(guard);
                live_outs
            },

            Triv(Use(id)) => { live_outs.insert(id); live_outs },

            Triv(Const(_)) => live_outs,

            Letrec(..) | Set(..) => unreachable!()
        }
    }

    live_ins(expr, anf::LiveVars::new());
}
