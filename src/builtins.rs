use std::fs;

use crate::native_fn::{NativeFn, Answer};
use crate::mutator::Mutator;
use crate::oref::Fixnum;
use crate::handle::HandleT;
use crate::bool::Bool;
use crate::string::String;
use crate::list::{Pair, EmptyList};
use crate::heap_obj::Singleton;
use crate::reader::Reader;
use crate::symbol::Symbol;
use crate::compiler::compile;
use crate::closure::Closure;
use crate::verifier::verify;

fn eq(mt: &mut Mutator) -> Answer {
    let res = mt.regs()[mt.regs().len() - 1] == mt.regs()[mt.regs().len() - 2];
    let res = Bool::instance(mt, res);
    mt.push(res.into());
    Answer::Ret
}

pub const EQ: NativeFn = NativeFn {
    arity: 3,
    code: eq
};

fn fx_sub(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_sub(b) {
                mt.push(v.into());
                Answer::Ret
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    } else {
        todo!()
    }
}

pub const FX_SUB: NativeFn = NativeFn {
    arity: 3,
    code: fx_sub
};

fn fx_mul(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_mul(b) {
                mt.push(v.into());
                Answer::Ret
            } else {
                todo!()
            }
        } else {
            todo!()
        }
    } else {
        todo!()
    }
}

pub const FX_MUL: NativeFn = NativeFn {
    arity: 3,
    code: fx_mul
};

fn eval(mt: &mut Mutator) -> Answer {
    let sexpr = mt.regs()[mt.regs().len() - 1];

    let code = compile(mt, sexpr, false);
    if let Err(err) = unsafe { verify(mt, code.as_ref()) } {
        todo!()
    }

    mt.push(code.into());
    let f = Closure::new(mt, 0);
    mt.pop();
    mt.push(f.into());
    Answer::TailCall {argc: 1}
}

pub const EVAL: NativeFn = NativeFn {
    arity: 2,
    code: eval
};

fn load(mt: &mut Mutator) -> Answer {
    let s = if let Some(filename) = mt.regs()[mt.regs().len() - 1].try_cast::<String>(mt) {
        unsafe { fs::read_to_string(filename.as_ref().as_str()).unwrap_or_else(|err|
            todo!()
        ) }
    } else {
        todo!()
    };

    let mut reader = Reader::new(&s);
    let mut builder: Option<(HandleT<Pair>, HandleT<Pair>)> = None;
    while let Some(res) = reader.next(mt) {
        match res {
            Ok(ssexpr) =>
                if let Some((_, ref mut last_pair)) = builder {
                    let nil = mt.root(EmptyList::instance(mt).into());
                    let pair = Pair::new(mt, ssexpr.v, nil);
                    unsafe { last_pair.as_mut().cdr = pair.into(); }
                    *last_pair = mt.root_t(pair);
                } else {
                    let nil = mt.root(EmptyList::instance(mt).into());
                    let pair = {
                        let pair = Pair::new(mt, ssexpr.v, nil);
                        mt.root_t(pair)
                    };
                    builder = Some((pair.clone(), pair));
                },
            Err(()) => todo!()
        }
    }
    let sexprs = match builder {
        Some((sexprs, _)) => sexprs.into(),
        None => mt.root(EmptyList::instance(mt).into())
    };

    let sexpr = { // `(begin ,@sexprs)
        let begin = {
            let begin = Symbol::new(mt, "begin");
            mt.root(begin.into())
        };
        let sexprs = Pair::new(mt, begin, sexprs);
        mt.root(sexprs.into())
    };

    mt.push_global("eval");
    mt.push(*sexpr);
    Answer::TailCall {argc: 2}
}

pub const LOAD: NativeFn = NativeFn {
    arity: 2,
    code: load
};
