use std::collections::hash_set::HashSet;

use crate::handle::Handle;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl From<usize> for Id {
    fn from(i: usize) -> Self { Self(i) }
}

pub type LiveVars = HashSet<Id>;

pub enum Triv {
    Use(Id),
    Const(Handle)
}

pub enum Expr {
    Let(Vec<Binding>, Box<Expr>, /* popnnt?: */ bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>, LiveVars),
    r#Fn(LiveVars, Params, Box<Expr>),
    Call(Id, Vec<Id>, LiveVars),
    Triv(Triv)
}

pub type Binding = (Id, Expr);

pub type Params = Vec<Id>;
