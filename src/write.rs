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
        unsafe {
            if let Some(this) = self.try_cast::<Symbol>(mt) {
                write!(fmt, "{}", this.as_ref().name())
            } else if let Some(this) = self.try_cast::<Pair>(mt) {
                write!(fmt, "({}", this.as_ref().car.within(mt))?;

                let mut ls = this.as_ref().cdr;
                loop {
                    if let Ok(ls_obj) = Gc::<()>::try_from(ls) {
                        if let Some(pair) = ls_obj.try_cast::<Pair>(mt) {
                            write!(fmt, " {}", pair.as_ref().car.within(mt))?;

                            ls = pair.as_ref().cdr;
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
            } else if let Some(this) = self.try_cast::<String>(mt) {
                write!(fmt, "\"{}\"", this.as_ref().as_str())
            } else if let Some(this) = self.try_cast::<Bool>(mt) {
                if bool::from(this.as_ref().0) {
                    write!(fmt, "#t")
                } else {
                    write!(fmt, "#f")
                }
            } else if let Some(vs) = self.try_cast::<Vector<ORef>>(mt) {
                write!(fmt, "#(")?;

                if vs.as_ref().indexed_field().len() > 0 {
                    write!(fmt, "{}", vs.as_ref().indexed_field()[0].within(mt))?;

                    for v in &vs.as_ref().indexed_field()[1..] {
                        write!(fmt, " {}", v.within(mt))?;
                    }
                }

                write!(fmt, ")")
            } else if let Some(_) = self.try_cast::<Closure>(mt) {
                write!(fmt, "#<fn @ {:p}>", self.as_ptr())
            } else if let Some(_) = self.try_cast::<NativeFn>(mt) {
                write!(fmt, "#<fn native @ {:p}>", self.as_ptr())
            } else if let Some(_) = self.try_cast::<Syntax>(mt) {
                write!(fmt, "#<syntax>") // TODO: show unwrapped .expr
            } else if let Some(pos) = self.try_cast::<Pos>(mt) {
                write!(fmt, "#<pos {} {} {}>",
                    pos.as_ref().filename.within(mt),
                    ORef::from(pos.as_ref().line).within(mt),
                    ORef::from(pos.as_ref().column).within(mt)
                )
            } else if let Some(_) = self.try_cast::<Box>(mt) {
                write!(fmt, "#<box>")
            } else if let Some(code) = self.try_cast::<Bytecode>(mt) {
                write!(fmt, "{}", code.within(mt))
            } else {
                write!(fmt, "#<object @ {:p}>", self.as_ptr())
            }
        }
    }
}
