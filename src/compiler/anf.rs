use std::collections::hash_set::HashSet;
use pretty::RcDoc;

use crate::oref::ORef;
use crate::handle::{Handle, HandleT};
use crate::compiler::{Compiler, Id};
use crate::symbol::Symbol;
use crate::mutator::Mutator;

pub type LiveVars = HashSet<Id>;

pub enum Triv {
    Use(Id),
    Const(Handle)
}

pub enum Expr {
    Define(HandleT<Symbol>, Box<PosExpr>),
    GlobalSet(HandleT<Symbol>, Box<PosExpr>),

    Begin(Vec<PosExpr>),
    Let(Vec<Binding>, Box<PosExpr>, LiveVars),
    Letrec(Vec<Binding>, Box<PosExpr>),

    If(Box<PosExpr>, Box<PosExpr>, Box<PosExpr>, LiveVars),

    Set(Id, Box<PosExpr>),
    Box(Box<PosExpr>),
    UninitializedBox,
    BoxSet(Id, Box<PosExpr>),
    CheckedBoxSet {guard: Id, r#box: Id, val_expr: Box<PosExpr>},
    BoxGet(Id),
    CheckedBoxGet {guard: Id, r#box: Id},

    r#Fn(LiveVars, Params, bool, Box<PosExpr>),
    Call(Vec<Binding>, LiveVars),
    CallWithValues((Id, Box<PosExpr>), (Id, Box<PosExpr>), LiveVars),

    Global(HandleT<Symbol>),
    CheckedUse {guard: Id, id: Id},
    Triv(Triv)
}

pub struct PosExpr {
    pub pos: Handle,
    pub expr: Expr
}

pub type Binding = (Id, PosExpr);

pub type Params = Vec<Id>;

impl PosExpr {
    pub fn to_doc<'a>(&'a self, mt: &'a Mutator, cmp: &Compiler) -> RcDoc<()> { self.expr.to_doc(mt, cmp) }
}

impl Expr {
    pub fn to_doc<'a>(&'a self, mt: &'a Mutator, cmp: &Compiler) -> RcDoc<()> {
        use Expr::*;
        use self::Triv::*;

        match self {
            &Define(ref sym, ref val_expr) =>
                RcDoc::text("(define")
                    .append(RcDoc::line().append(RcDoc::as_string(ORef::from(sym.oref()).within(mt))).nest(2))
                    .group()
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &GlobalSet(ref sym, ref val_expr) =>
                RcDoc::text("(global-set!")
                    .append(RcDoc::line().append(RcDoc::as_string(ORef::from(sym.oref()).within(mt))).nest(2))
                    .group()
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &Begin(ref stmts) =>
                RcDoc::text("(begin")
                    .append(RcDoc::line()
                        .append(RcDoc::intersperse(stmts.iter().map(|stmt| stmt.to_doc(mt, cmp)), RcDoc::line()))
                            .nest(2))
                    .append(RcDoc::text(")"))
                    .group(),

            &Let(ref bindings, ref body, _) =>
                RcDoc::text("(let")
                    .append(RcDoc::line().append(RcDoc::text("("))
                        .append(RcDoc::intersperse(bindings.iter().map(|b| binding_to_doc(b, mt, cmp)), RcDoc::line())
                            .nest(1))
                        .append(RcDoc::text(")")).append(RcDoc::line())
                        .append(body.to_doc(mt, cmp).nest(2)).append(RcDoc::text(")"))
                        .nest(2)),

            &Letrec(ref bindings, ref body) =>
                RcDoc::text("(letrec")
                    .append(RcDoc::line().append(RcDoc::text("("))
                        .append(RcDoc::intersperse(bindings.iter().map(|b| binding_to_doc(b, mt, cmp)), RcDoc::line())
                            .nest(1))
                        .append(RcDoc::text(")")).append(RcDoc::line())
                        .append(body.to_doc(mt, cmp).nest(2)).append(RcDoc::text(")"))
                        .nest(2)),

            &If(ref cond, ref conseq, ref alt, _) =>
                RcDoc::text("(if")
                    .append(RcDoc::line().append(cond.to_doc(mt, cmp)).nest(2))
                    .group()
                    .append(RcDoc::line().append(conseq.to_doc(mt, cmp)).nest(2))
                    .append(RcDoc::line().append(alt.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &Set(id, ref val_expr) =>
                RcDoc::text("(set!")
                    .append(RcDoc::line().append(id.to_doc(cmp)).nest(2))
                    .group()
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &Box(ref val_expr) =>
                RcDoc::text("(box")
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &UninitializedBox => RcDoc::text("(uninitialized-box)"),

            &BoxSet(id, ref val_expr) =>
                RcDoc::text("(box-set!")
                    .append(RcDoc::line().append(id.to_doc(cmp)).nest(2))
                    .group()
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &CheckedBoxSet {guard, r#box, ref val_expr} =>
                RcDoc::text("(checked-box-set!")
                    .append(RcDoc::line().append(guard.to_doc(cmp)).nest(2))
                    .append(RcDoc::line().append(r#box.to_doc(cmp)).nest(2))
                    .group()
                    .append(RcDoc::line().append(val_expr.to_doc(mt, cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &BoxGet(id) =>
                RcDoc::text("(box-get")
                    .append(RcDoc::line().append(id.to_doc(cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &CheckedBoxGet {guard, r#box} =>
                RcDoc::text("(checked-box-get")
                    .append(RcDoc::line().append(guard.to_doc(cmp)).nest(2))
                    .group()
                    .append(RcDoc::line().append(r#box.to_doc(cmp)).nest(2)).append(RcDoc::text(")"))
                    .group(),

            &Fn(_, ref params, varargs, ref body) =>
                RcDoc::text("(lambda")
                    .append(RcDoc::line().append(params_to_doc(params, varargs, cmp))
                        .nest(2))
                    .group()
                    .append(RcDoc::line().append(body.to_doc(mt, cmp))
                        .nest(2)).append(RcDoc::text(")"))
                    .group(),

            &Call(ref cargs, _) =>
                RcDoc::text("(call")
                    .append(RcDoc::line()
                        .append(RcDoc::intersperse(cargs.iter().map(|b| binding_to_doc(b, mt, cmp)), RcDoc::line()))
                        .nest(1)).append(RcDoc::text(")"))
                    .group(),

            &CallWithValues((pid, ref producer), (cid, ref consumer), _) =>
                RcDoc::text("(call-with-values*")
                    .append(RcDoc::line()
                        .append(RcDoc::text("(")
                            .append(pid.to_doc(cmp).append(RcDoc::line())
                                .append(producer.to_doc(mt, cmp))
                                .group().nest(1))
                            .append(RcDoc::text(")")))
                        .append(RcDoc::text("(")
                            .append(cid.to_doc(cmp).append(RcDoc::line())
                                .append(consumer.to_doc(mt, cmp))
                                .group().nest(1))
                            .append(RcDoc::text(")")))
                        .nest(1)).append(RcDoc::text(")"))
                    .group(),

            &Global(ref sym) => RcDoc::as_string(ORef::from(sym.oref()).within(mt)),

            &CheckedUse {guard, id} =>
                RcDoc::text("(checked-use").append(RcDoc::line())
                    .append(guard.to_doc(cmp)).append(RcDoc::line())
                    .append(id.to_doc(cmp)).append(RcDoc::text(")"))
                    .group(),

            &Triv(Use(id)) => id.to_doc(cmp),

            &Triv(Const(ref c)) => c.oref().to_doc(mt)
        }
    }
}

fn binding_to_doc<'a>(&(id, ref val_expr): &'a Binding, mt: &'a Mutator, cmp: &Compiler) -> RcDoc<'a, ()> {
    RcDoc::text("(")
        .append(id.to_doc(cmp).append(RcDoc::line())
            .append(val_expr.to_doc(mt, cmp))
            .group().nest(1))
        .append(RcDoc::text(")"))
}

fn params_to_doc<'a>(params: &[Id], varargs: bool, cmp: &Compiler) -> RcDoc<'a, ()> {
    let doc = if varargs {
        let min_arity = params.len() - 1; // > 0 due to 'self' closure
        RcDoc::intersperse(params[0..min_arity].iter().map(|param| param.to_doc(cmp)), RcDoc::line())
            .append(RcDoc::line()).append(RcDoc::text(".")).append(RcDoc::line())
            .append(params[min_arity].to_doc(cmp))
    } else {
        RcDoc::intersperse(params.iter().map(|param| param.to_doc(cmp)), RcDoc::line())
    };

    RcDoc::text("(").append(doc.group().nest(1)).append(RcDoc::text(")"))
}
