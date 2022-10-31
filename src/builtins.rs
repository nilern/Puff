use std::fs;

use crate::native_fn::{NativeFn, Answer};
use crate::mutator::Mutator;
use crate::oref::{ORef, Fixnum, Gc};
use crate::handle::{HandleT, Root, root};
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
    Answer::Ret {retc: 1}
}

pub const EQ: NativeFn = NativeFn {
    min_arity: 3,
    varargs: false,
    code: eq
};

fn fx_add(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_add(b) {
                mt.push(v.into());
                Answer::Ret {retc: 1}
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
    min_arity: 3,
    varargs: false,
    code: fx_add
};

fn fx_sub(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_sub(b) {
                mt.push(v.into());
                Answer::Ret {retc: 1}
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
    min_arity: 3,
    varargs: false,
    code: fx_sub
};

fn fx_mul(mt: &mut Mutator) -> Answer {
    if let Ok(b) = Fixnum::try_from(mt.regs()[mt.regs().len() - 1]) {
        if let Ok(a) = Fixnum::try_from(mt.regs()[mt.regs().len() - 2]) {
            if let Some(v) = a.checked_mul(b) {
                mt.push(v.into());
                Answer::Ret {retc: 1}
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
    min_arity: 3,
    varargs: false,
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
    Answer::Ret {retc: 1}
}

pub const IS_PAIR: NativeFn = NativeFn {
    min_arity: 2,
    varargs: false,
    code: is_pair
};

fn is_null(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let res = mt.regs()[last_index] == EmptyList::instance(mt).into();

    mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
    Answer::Ret {retc: 1}
}

pub const IS_NULL: NativeFn = NativeFn {
    min_arity: 2,
    varargs: false,
    code: is_null
};

fn cons(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = unsafe {
        let nptr = mt.alloc_static::<Pair>();
        nptr.as_ptr().write(Pair::new(mt.regs()[last_index - 1], mt.regs()[last_index]));
        Gc::new_unchecked(nptr)
    };

    mt.regs_mut()[last_index] = pair.into();
    Answer::Ret {retc: 1}
}

pub const CONS: NativeFn = NativeFn {
    min_arity: 3,
    varargs: false,
    code: cons
};

fn car(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );

    unsafe { mt.regs_mut()[last_index] = pair.as_ref().car(); }
    Answer::Ret {retc: 1}
}

pub const CAR: NativeFn = NativeFn {
    min_arity: 2,
    varargs: false,
    code: car
};

fn cdr(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );

    unsafe { mt.regs_mut()[last_index] = pair.as_ref().cdr(); }
    Answer::Ret {retc: 1}
}

pub const CDR: NativeFn = NativeFn {
    min_arity: 2,
    varargs: false,
    code: cdr
};

fn set_car(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index - 1].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );
    let v = mt.regs()[last_index];

    unsafe { pair.as_ref().set_car(v); }

    Answer::Ret {retc: 1} // HACK: Happens to return `v`
}

pub const SET_CAR: NativeFn = NativeFn {
    min_arity: 3,
    varargs: false,
    code: set_car
};

fn set_cdr(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let pair = mt.regs()[last_index - 1].try_cast::<Pair>(mt).unwrap_or_else(||
        todo!()
    );
    let v = mt.regs()[last_index];

    unsafe { pair.as_ref().set_cdr(v); }

    Answer::Ret {retc: 1} // HACK: Happens to return `v`
}

pub const SET_CDR: NativeFn = NativeFn {
    min_arity: 3,
    varargs: false,
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
    min_arity: 2,
    varargs: false,
    code: eval_syntax
};

fn load(mt: &mut Mutator) -> Answer {
    let (s, filename) = if let Some(filename) = mt.regs()[mt.regs().len() - 1].try_cast::<String>(mt) {
        (unsafe { fs::read_to_string(filename.as_ref().as_str()).unwrap_or_else(|_|
             todo!()
         ) },
         root!(mt, filename))
    } else {
        todo!()
    };

    let mut reader = Reader::new(&s, Some(filename.clone()));
    let mut builder: Option<(HandleT<Pair>, HandleT<Pair>)> = None;
    while let Some(res) = reader.next(mt) {
        match res {
            Ok(stx) =>
                if let Some((_, ref mut last_pair)) = builder {
                    let nil = root!(mt, ORef::from(EmptyList::instance(mt)));
                    let pair = Gc::<Pair>::new(mt, stx.into(), nil);
                    unsafe { last_pair.as_ref().set_cdr(pair.into()); }
                    *last_pair = root!(mt, pair);
                } else {
                    let nil = root!(mt, ORef::from(EmptyList::instance(mt)));
                    let pair = root!(mt, Gc::<Pair>::new(mt, stx.into(), nil));
                    builder = Some((pair.clone(), pair));
                },
            Err(()) => todo!()
        }
    }
    let sexprs = match builder {
        Some((sexprs, _)) => sexprs.into(),
        None => root!(mt, ORef::from(EmptyList::instance(mt)))
    };

    let start = root!(mt, Pos::new(mt, Some(filename), Fixnum::from(1u8), Fixnum::from(1u8)));
    let sexpr = { // `(begin ,@sexprs)
        let begin = {
            let begin = root!(mt, Symbol::new(mt, "begin"));
            root!(mt, Syntax::new(mt, begin.into(), Some(start.clone())))
        };
        root!(mt, Gc::<Pair>::new(mt, begin.into(), sexprs))
    };
    let stx = root!(mt, Syntax::new(mt, sexpr.into(), Some(start)));

    mt.push_global("eval-syntax");
    mt.push((*stx).into());
    Answer::TailCall {argc: 2}
}

pub const LOAD: NativeFn = NativeFn {
    min_arity: 2,
    varargs: false,
    code: load
};

fn apply(mt: &mut Mutator) -> Answer {
    let mut argc = mt.regs().len() - 2;

    let mut arglist = mt.regs_mut().pop().unwrap();
    while let Some(args_pair) = arglist.try_cast::<Pair>(mt) {
        mt.push(unsafe { args_pair.as_ref().car() });
        argc += 1;
        arglist = unsafe { args_pair.as_ref().cdr() };
    }

    if arglist != EmptyList::instance(mt).into() {
        todo!() // error
    }

    Answer::TailCall {argc}
}

pub const APPLY: NativeFn = NativeFn {
    min_arity: 3,
    varargs: true,
    code: apply 
};

fn values(mt: &mut Mutator) -> Answer {
    Answer::Ret {retc: mt.regs().len() - 1}
}

pub const VALUES: NativeFn = NativeFn {
    min_arity: 1,
    varargs: true,
    code: values
};
