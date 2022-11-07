use std::fs;

use crate::native_fn::{NativeFn, Answer};
use crate::mutator::Mutator;
use crate::oref::{ORef, Gc};
use crate::handle::{Handle, Root, root};
use crate::fixnum::Fixnum;
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

fn type_of(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let t = mt.regs()[last_index].r#type();

    mt.regs_mut()[last_index] = t.into();
    Answer::Ret {retc: 1}
}

pub const TYPE_OF: NativeFn = NativeFn { min_arity: 2, varargs: false, code: type_of };

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

    mt.regs_mut()[last_index] = mt.borrow(pair).car();
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

    mt.regs_mut()[last_index] = mt.borrow(pair).cdr();
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

    mt.borrow(pair).set_car(v);

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

    mt.borrow(pair).set_cdr(v);

    Answer::Ret {retc: 1} // HACK: Happens to return `v`
}

pub const SET_CDR: NativeFn = NativeFn {
    min_arity: 3,
    varargs: false,
    code: set_cdr
};

fn eval_syntax(mt: &mut Mutator) -> Answer {
    let expr = root!(mt, mt.regs()[mt.regs().len() - 1]);

    if mt.cfg().debug {
        println!("{}", expr.oref().within(&mt));
        println!("");
    }

    let code = compile(mt, expr, mt.cfg().debug);

    if mt.cfg().debug {
        println!("{}", code.within(&mt));
        println!("");
    }

    if let Err(err) = verify(mt, mt.borrow(code)) {
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
        (fs::read_to_string(mt.borrow(filename).as_str()).unwrap_or_else(|_|
             todo!()
         ),
         root!(mt, filename))
    } else {
        todo!()
    };

    let mut reader = Reader::new(&s, Some(filename.clone()));
    let mut builder: Option<(Handle<Pair>, Handle<Pair>)> = None;
    while let Some(res) = reader.next(mt) {
        match res {
            Ok(stx) =>
                if let Some((_, ref mut last_pair)) = builder {
                    let nil = root!(mt, ORef::from(EmptyList::instance(mt)));
                    let pair = Gc::<Pair>::new(mt, stx.borrow().into(), nil.borrow());
                    last_pair.set_cdr(pair.into());
                    *last_pair = root!(mt, pair);
                } else {
                    let nil = root!(mt, ORef::from(EmptyList::instance(mt)));
                    let pair = root!(mt, Gc::<Pair>::new(mt, stx.borrow().into(), nil.borrow()));
                    builder = Some((pair.clone(), pair));
                },
            Err(()) => todo!()
        }
    }
    let sexprs = match builder {
        Some((sexprs, _)) => sexprs.into(),
        None => root!(mt, ORef::from(EmptyList::instance(mt)))
    };

    let start = root!(mt, Pos::new(mt, Some(filename.borrow()), Fixnum::from(1u8), Fixnum::from(1u8)));
    let sexpr = { // `(begin ,@sexprs)
        let begin = {
            let begin = root!(mt, Symbol::new(mt, "begin"));
            root!(mt, Syntax::new(mt, begin.borrow().into(), Some(start.borrow())))
        };
        root!(mt, Gc::<Pair>::new(mt, begin.borrow().into(), sexprs.borrow()))
    };
    let stx = root!(mt, Syntax::new(mt, sexpr.borrow().into(), Some(start.borrow())));

    mt.push_global("eval-syntax");
    mt.push(stx.oref().into());
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
        mt.push(mt.borrow(args_pair).car());
        argc += 1;
        arglist = mt.borrow(args_pair).cdr();
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
