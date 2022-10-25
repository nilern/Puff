use std::collections::hash_set::HashSet;

use crate::handle::{Handle, HandleT};
use crate::compiler::Id;
use crate::symbol::Symbol;

pub type LiveVars = HashSet<Id>;

pub enum Triv {
    Use(Id),
    Const(Handle)
}

pub enum Expr {
    Define(HandleT<Symbol>, Box<Expr>),
    GlobalSet(HandleT<Symbol>, Box<Expr>),

    Begin(Vec<Expr>),
    Let(Vec<Binding>, Box<Expr>, /* popnnt?: */ bool),
    Letrec(Vec<Binding>, Box<Expr>),

    If(Box<Expr>, Box<Expr>, Box<Expr>, LiveVars),

    Set(Id, Box<Expr>),
    Box(Box<Expr>),
    UninitializedBox,
    BoxSet(Id, Box<Expr>),
    CheckedBoxSet {guard: Id, r#box: Id, val_expr: Box<Expr>},
    BoxGet(Id),
    CheckedBoxGet {guard: Id, r#box: Id},

    r#Fn(LiveVars, Params, bool, Box<Expr>),
    Call(Id, Vec<Id>, LiveVars),

    Global(HandleT<Symbol>),
    CheckedUse {guard: Id, id: Id},
    Triv(Triv)
}

pub type Binding = (Id, Expr);

pub type Params = Vec<Id>;
