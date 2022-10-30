use std::fs;

use crate::native_fn::{NativeFn, Answer};
use crate::mutator::Mutator;
use crate::oref::{Fixnum, Gc};
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
use crate::syntax::{Pos, Syntax};

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

fn fx_add(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_add(b) {
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

pub const FX_ADD: NativeFn = NativeFn {
    arity: 3,
    code: fx_add
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

fn is_pair(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let res = if let Ok(obj) = Gc::<()>::try_from(mt.regs()[last_index]) {
        obj.instance_of::<Pair>(mt)
    } else {
        false
    };

    mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
    Answer::Ret
}

pub const IS_PAIR: NativeFn = NativeFn {
    arity: 2,
    code: is_pair
};

fn is_null(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let res = mt.regs()[last_index] == EmptyList::instance(mt).into();

    mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
    Answer::Ret
}

pub const IS_NULL: NativeFn = NativeFn {
    arity: 2,
    code: is_null
};

fn cons(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = unsafe { mt.alloc_static::<Pair>() };
    unsafe { pair.as_ptr().write(Pair {
        car: mt.regs()[last_index - 1],
        cdr: mt.regs()[last_index]
    }) };

    unsafe { mt.regs_mut()[last_index] = Gc::new_unchecked(pair).into(); }
    Answer::Ret
}

pub const CONS: NativeFn = NativeFn {
    arity: 3,
    code: cons
};

fn car(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );

    unsafe { mt.regs_mut()[last_index] = pair.as_ref().car; }
    Answer::Ret
}

pub const CAR: NativeFn = NativeFn {
    arity: 2,
    code: car
};

fn cdr(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );

    unsafe { mt.regs_mut()[last_index] = pair.as_ref().cdr; }
    Answer::Ret
}

pub const CDR: NativeFn = NativeFn {
    arity: 2,
    code: cdr
};

fn set_car(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let mut pair = mt.regs()[last_index - 1].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );
    let v = mt.regs()[last_index];

    unsafe { pair.as_mut().car = v; }

    Answer::Ret // HACK: Happens to return `v`
}

pub const SET_CAR: NativeFn = NativeFn {
    arity: 3,
    code: set_car
};

fn set_cdr(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let mut pair = mt.regs()[last_index - 1].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );
    let v = mt.regs()[last_index];

    unsafe { pair.as_mut().cdr = v; }

    Answer::Ret // HACK: Happens to return `v`
}

pub const SET_CDR: NativeFn = NativeFn {
    arity: 3,
    code: set_cdr
};

fn eval_syntax(mt: &mut Mutator) -> Answer {
    let expr = mt.regs()[mt.regs().len() - 1];

    if mt.cfg().debug {
        println!("{}", expr.within(&mt));
        println!("");
    }

    let code = compile(mt, expr, mt.cfg().debug);

    if mt.cfg().debug {
        println!("{}", code.within(&mt));
        println!("");
    }

    if let Err(err) = unsafe { verify(mt, code.as_ref()) } {
        if mt.cfg().debug {
            println!("VerificationError: {:?}", err);
        }

        todo!()
    }

    mt.push(code.into());
    let f = Closure::new(mt, 0);
    mt.pop();
    mt.push(f.into());
    Answer::TailCall {argc: 1}
}

pub const EVAL_SYNTAX: NativeFn = NativeFn {
    arity: 2,
    code: eval_syntax
};

fn load(mt: &mut Mutator) -> Answer {
    let (s, filename) = if let Some(filename) = mt.regs()[mt.regs().len() - 1].try_cast::<String>(mt) {
        (unsafe { fs::read_to_string(filename.as_ref().as_str()).unwrap_or_else(|_|
             todo!()
         ) },
         mt.root_t(filename))
    } else {
        todo!()
    };

    let mut reader = Reader::new(&s, Some(filename.clone()));
    let mut builder: Option<(HandleT<Pair>, HandleT<Pair>)> = None;
    while let Some(res) = reader.next(mt) {
        match res {
            Ok(stx) =>
                if let Some((_, ref mut last_pair)) = builder {
                    let nil = mt.root(EmptyList::instance(mt).into());
                    let pair = Pair::new(mt, stx.into(), nil);
                    unsafe { last_pair.as_mut().cdr = pair.into(); }
                    *last_pair = mt.root_t(pair);
                } else {
                    let nil = mt.root(EmptyList::instance(mt).into());
                    let pair = {
                        let pair = Pair::new(mt, stx.into(), nil);
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

    let start = {
        let start = Pos::new(mt, Some(filename), Fixnum::from(1u8), Fixnum::from(1u8));
        mt.root_t(start)
    };
    let sexpr = { // `(begin ,@sexprs)
        let begin = {
            let begin = Symbol::new(mt, "begin");
            let begin = mt.root(begin.into());
            let begin = Syntax::new(mt, begin, Some(start.clone()));
            mt.root(begin.into())
        };
        let sexprs = Pair::new(mt, begin, sexprs);
        mt.root(sexprs.into())
    };
    let stx = {
        let stx = Syntax::new(mt, sexpr, Some(start));
        mt.root_t(stx)
    };

    mt.push_global("eval-syntax");
    mt.push((*stx).into());
    Answer::TailCall {argc: 2}
}

pub const LOAD: NativeFn = NativeFn {
    arity: 2,
    code: load
};
