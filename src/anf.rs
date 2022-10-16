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

    Let(Vec<Binding>, Box<Expr>, /* popnnt?: */ bool),

    If(Box<Expr>, Box<Expr>, Box<Expr>, LiveVars),

    Set(Id, Box<Expr>),
    Box(Box<Expr>),
    BoxSet(Id, Box<Expr>),
    BoxGet(Id),

    r#Fn(LiveVars, Params, Box<Expr>),
    Call(Id, Vec<Id>, LiveVars),

    Global(HandleT<Symbol>),
    Triv(Triv)
}

pub type Binding = (Id, Expr);

pub type Params = Vec<Id>;
