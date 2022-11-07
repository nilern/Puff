use std::fmt::{self, Display};

use crate::oref::{ORef, Gc, ORefEnum};
use crate::heap_obj::Indexed;
use crate::bytecode::Bytecode;
use crate::syntax::{Syntax, Pos};
use crate::closure::Closure;
use crate::native_fn::NativeFn;
use crate::vector::Vector;
use crate::bool::Bool;
use crate::list::{Pair, EmptyList};
use crate::mutator::{Mutator, WithinMt};
use crate::symbol::Symbol;
use crate::string::String;
use crate::r#box::Box;
use crate::r#type::Type;

pub trait DisplayWithin {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result;
}

impl<'a, T: DisplayWithin> Display for WithinMt<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result { self.v.fmt_within(self.mt, fmt) }
}

impl DisplayWithin for ORef {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result {
        match (*self).into() {
            ORefEnum::Gc(obj) => obj.fmt_within(mt, fmt),
            ORefEnum::Fixnum(n) => write!(fmt, "{}", n),
            ORefEnum::Flonum(f) => write!(fmt, "{}", f),
            ORefEnum::Char(c) => write!(fmt, "{}", c)
        }
    }
}

impl DisplayWithin for Gc<()> {
    fn fmt_within(&self, mt: &Mutator, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Some(sym) = self.try_cast::<Symbol>(mt) {
            write!(fmt, "{}", mt.borrow(sym).name())
        } else if let Some(pair) = self.try_cast::<Pair>(mt) {
            let pair = mt.borrow(pair);
            write!(fmt, "({}", pair.car().within(mt))?;

            let mut ls = pair.cdr();
            loop {
                if let Ok(ls_obj) = Gc::<()>::try_from(ls) {
                    if let Some(pair) = ls_obj.try_cast::<Pair>(mt) {
                        let pair = mt.borrow(pair);
                        write!(fmt, " {}", pair.car().within(mt))?;

                        ls = pair.cdr();
                        continue;
                    } else if let Some(_) = ls_obj.try_cast::<EmptyList>(mt) {
                        break;
                    }
                }

                write!(fmt, " . {}", ls.within(mt))?;
                break;
            }

            write!(fmt, ")")
        } else if let Some(_) = self.try_cast::<EmptyList>(mt) {
            write!(fmt, "()")
        } else if let Some(s) = self.try_cast::<String>(mt) {
            write!(fmt, "\"{}\"", mt.borrow(s).as_str())
        } else if let Some(b) = self.try_cast::<Bool>(mt) {
            if bool::from(mt.borrow(b).0) {
                write!(fmt, "#t")
            } else {
                write!(fmt, "#f")
            }
        } else if let Some(vs) = self.try_cast::<Vector<ORef>>(mt) {
            write!(fmt, "#(")?;

            let vs = mt.borrow(vs);
            if vs.indexed_field().len() > 0 {
                write!(fmt, "{}", vs.indexed_field()[0].within(mt))?;

                for v in &vs.indexed_field()[1..] {
                    write!(fmt, " {}", v.within(mt))?;
                }
            }

            write!(fmt, ")")
        } else if let Some(_) = self.try_cast::<Type>(mt) {
            write!(fmt, "#<type>")
        } else if let Some(_) = self.try_cast::<Closure>(mt) {
            write!(fmt, "#<fn @ {:p}>", mt.borrow(*self))
        } else if let Some(_) = self.try_cast::<NativeFn>(mt) {
            write!(fmt, "#<fn native @ {:p}>", mt.borrow(*self))
        } else if let Some(_) = self.try_cast::<Syntax>(mt) {
            write!(fmt, "#<syntax>") // TODO: show unwrapped .expr
        } else if let Some(pos) = self.try_cast::<Pos>(mt) {
            let pos = mt.borrow(pos);
            write!(fmt, "#<pos {} {} {}>",
                pos.filename.within(mt),
                ORef::from(pos.line).within(mt),
                ORef::from(pos.column).within(mt)
            )
        } else if let Some(_) = self.try_cast::<Box>(mt) {
            write!(fmt, "#<box>")
        } else if let Some(code) = self.try_cast::<Bytecode>(mt) {
            write!(fmt, "{}", code.within(mt))
        } else {
            write!(fmt, "#<object @ {:p}>", mt.borrow(*self))
        }
    }
}
