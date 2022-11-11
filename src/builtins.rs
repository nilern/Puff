use std::fs;

use crate::heap_obj::Header;
use crate::native_fn::{NativeFn, Answer};
use crate::mutator::Mutator;
use crate::oref::{ORef, Gc};
use crate::handle::{Handle, Root, root};
use crate::fixnum::Fixnum;
use crate::bool::Bool;
use crate::string::{String, StringMut};
use crate::char::Char;
use crate::list::{Pair, EmptyList};
use crate::heap_obj::Singleton;
use crate::reader::Reader;
use crate::symbol::Symbol;
use crate::compiler::compile;
use crate::closure::Closure;
use crate::verifier::verify;
use crate::syntax::{Pos, Syntax};
use crate::r#type::{Type, NonIndexedType, IndexedType};

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

fn is_instance(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let t = mt.regs()[last_index - 1].try_cast::<Type>(mt).unwrap_or_else(|| {
        todo!() // error
    });
    let v = mt.regs()[last_index];
    let res = v.instance_of_dyn(mt, t);

    mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
    Answer::Ret {retc: 1}
}

pub const IS_INSTANCE: NativeFn = NativeFn {min_arity: 3, varargs: false, code: is_instance};

fn make_zeroed(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let t = mt.regs()[last_index].try_cast::<Type>(mt).unwrap_or_else(|| {
        todo!("error: not a type")
    });

    let res = if t.oref_zeroable(mt) {
        ORef::from(Fixnum::from(0u8))
    } else if mt.borrow(t).zeroable {
        match Gc::<NonIndexedType>::try_from(t) {
            Ok(t) => ORef::from(unsafe { Gc::new_unchecked(mt.alloc_nonindexed(t)) }),
            Err(()) => todo!("error: indexed type")
        }
    } else {
        todo!("error: nonzeroable")
    };

    mt.regs_mut()[last_index] = res;
    Answer::Ret {retc: 1}
}

pub const MAKE_ZEROED: NativeFn = NativeFn {min_arity: 2, varargs: false, code: make_zeroed};

fn make_indexed_zeroed(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let t = mt.regs()[last_index - 1].try_cast::<Type>(mt).unwrap_or_else(|| {
        todo!("error: not a type")
    });
    let len = match Fixnum::try_from(mt.regs()[last_index]) {
        Ok(len) => {
            let len = isize::from(len);
            if len >= 0 {
                len as usize
            } else {
                todo!()
            }
        },

        Err(()) => todo!()
    };

    let res = match Gc::<IndexedType>::try_from(t) {
        Ok(it) =>
            if mt.borrow(t).zeroable {
                ORef::from(unsafe { Gc::new_unchecked(mt.alloc_indexed(it, len)) })
            } else {
                todo!("error: nonzeroable")
            },

        Err(()) => todo!("error: nonindexed type")
    };

    mt.regs_mut()[last_index] = res;
    Answer::Ret {retc: 1}
}

pub const MAKE_INDEXED_ZEROED: NativeFn = NativeFn {min_arity: 3, varargs: false, code: make_indexed_zeroed};

fn make(mt: &mut Mutator) -> Answer {
    let t = mt.regs()[1].try_cast::<Type>(mt).unwrap_or_else(|| {
        todo!("error: not a type")
    });

    if mt.borrow(t).is_bits {
        todo!("error: bits type");
    }

    let field_argc = mt.regs().len() - 2;

    let obj = if !mt.borrow(t).has_indexed {
        if field_argc != mt.borrow(t).fields().len() {
            todo!("error: field_argc");
        }

        let obj = unsafe { mt.alloc_nonindexed(t.unchecked_cast::<NonIndexedType>()) };
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() }; // Reload in case GC happened

        for (i, field_descr) in mt.borrow(t).fields().iter().enumerate() {
            let v = mt.regs()[i + 2];

            if !v.instance_of_dyn(mt, field_descr.r#type) {
                todo!("error: field type");
            }

            if !mt.borrow(field_descr.r#type).inlineable {
                unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef) = v; }
            } else {
                todo!()
            }
        }

        unsafe { Gc::new_unchecked(obj) }
    } else {
        let non_indexed_fieldc = mt.borrow(t).fields().len() - 1;

        if field_argc < non_indexed_fieldc {
            todo!("error: field_argc");
        }

        let indexed_len = field_argc - non_indexed_fieldc;
        let obj = unsafe { mt.alloc_indexed(t.unchecked_cast::<IndexedType>(), indexed_len) };
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() }; // Reload in case GC happened

        for (i, field_descr) in mt.borrow(t).fields()[0..non_indexed_fieldc].iter().enumerate() {
            let v = mt.regs()[2 + i];

            if !v.instance_of_dyn(mt, field_descr.r#type) {
                todo!("error: field type");
            }

            if !mt.borrow(field_descr.r#type).inlineable {
                unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef) = v; }
            } else {
                todo!()
            }
        }

        let field_descr = mt.borrow(t).fields()[non_indexed_fieldc];
        for i in 0..indexed_len {
            let v = mt.regs()[2 + non_indexed_fieldc + i];

            if !v.instance_of_dyn(mt, field_descr.r#type) {
                todo!("error: field type");
            }

            if !mt.borrow(field_descr.r#type).inlineable {
                unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef).add(i) = v; }
            } else {
                todo!()
            }
        }

        unsafe { Gc::new_unchecked(obj) }
    };

    let last_index = mt.regs_mut().len() - 1;
    mt.regs_mut()[last_index] = obj.into();
    Answer::Ret {retc: 1}
}

pub const MAKE: NativeFn = NativeFn {min_arity: 2, varargs: true, code: make};

fn field_ref(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index - 1]).unwrap_or_else(|_| {
        todo!() // error
    });
    let index = isize::from(Fixnum::try_from(mt.regs()[last_index]).unwrap_or_else(|_| {
        todo!() // error
    }));
    let index = if index >= 0 {
        index as usize
    } else {
        todo!() // error
    };

    let t = mt.borrow(obj.r#type());

    if t.has_indexed && index == t.fields().len() - 1 {
        todo!() // Error: indexed field
    }

    let field_descr = t.fields().get(index).unwrap_or_else(|| {
        todo!() // Error: field index out of bounds
    });

    let v = if !mt.borrow(field_descr.r#type).inlineable {
        unsafe { *((obj.as_ptr() as *const u8).add(field_descr.offset) as *const ORef) }
    } else {
        todo!()
    };

    mt.regs_mut()[last_index] = v;
    Answer::Ret {retc: 1}
}

pub const FIELD_REF: NativeFn = NativeFn { min_arity: 3, varargs: false, code: field_ref };

fn field_set(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index - 2]).unwrap_or_else(|_| {
        todo!() // error
    });
    let index = isize::from(Fixnum::try_from(mt.regs()[last_index - 1]).unwrap_or_else(|_| {
        todo!() // error
    }));
    let index = if index >= 0 {
        index as usize
    } else {
        todo!() // error
    };
    let v = mt.regs()[last_index];

    let t = mt.borrow(obj.r#type());

    if t.has_indexed && index == t.fields().len() - 1 {
        todo!() // Error: indexed field
    }

    let field_descr = t.fields().get(index).unwrap_or_else(|| {
        todo!() // Error: field index out of bounds
    });

    if !v.instance_of_dyn(mt, field_descr.r#type) {
        todo!() // error
    }

    if !mt.borrow(field_descr.r#type).inlineable {
        unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef) = v; }
    } else {
        todo!()
    }

    Answer::Ret {retc: 1} // HACK: happens to return `v`
}

pub const FIELD_SET: NativeFn = NativeFn {min_arity: 4, varargs: false, code: field_set};

fn indexed_length(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index]).unwrap_or_else(|_| {
        todo!() // error
    });

    let t = mt.borrow(obj.r#type());

    if !t.has_indexed {
        todo!() // Error: no indexed field
    }

    let res = Fixnum::try_from(unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) })
        .unwrap_or_else(|_| todo!() /* error */);

    mt.regs_mut()[last_index] = res.into();
    Answer::Ret {retc: 1}
}

pub const INDEXED_LENGTH: NativeFn = NativeFn {min_arity: 2, varargs: false, code: indexed_length};

fn indexed_ref(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index - 1]).unwrap_or_else(|_| {
        todo!() // error
    });
    let index = isize::from(Fixnum::try_from(mt.regs()[last_index]).unwrap_or_else(|_| {
        todo!() // error
    }));
    let index = if index >= 0 {
        index as usize
    } else {
        todo!() // error
    };

    let t = mt.borrow(obj.r#type());

    if !t.has_indexed {
        todo!() // Error: no indexed field
    }

    let field_descr = t.fields()[t.fields().len() - 1];

    if index >= unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) } {
        todo!() // Error: out of bounds
    }

    let v = if !mt.borrow(field_descr.r#type).inlineable {
        unsafe { *((obj.as_ptr() as *const u8).add(field_descr.offset) as *const ORef).add(index) }
    } else {
        todo!()
    };

    mt.regs_mut()[last_index] = v;
    Answer::Ret {retc: 1}
}

pub const INDEXED_REF: NativeFn = NativeFn { min_arity: 3, varargs: false, code: indexed_ref };

fn indexed_set(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index - 2]).unwrap_or_else(|_| {
        todo!() // error
    });
    let index = isize::from(Fixnum::try_from(mt.regs()[last_index - 1]).unwrap_or_else(|_| {
        todo!() // error
    }));
    let index = if index >= 0 {
        index as usize
    } else {
        todo!() // error
    };
    let v = mt.regs()[last_index];

    let t = mt.borrow(obj.r#type());

    if !t.has_indexed {
        todo!() // Error: no indexed field
    }

    let field_descr = t.fields()[t.fields().len() - 1];

    if index >= unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) } {
        todo!() // Error: out of bounds
    }

    if !v.instance_of_dyn(mt, field_descr.r#type) {
        todo!() // error
    }

    if !mt.borrow(field_descr.r#type).inlineable {
        unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef).add(index) = v; }
    } else {
        todo!()
    }

    Answer::Ret {retc: 1} // HACK: happens to return `v`
}

pub const INDEXED_SET: NativeFn = NativeFn {min_arity: 4, varargs: false, code: indexed_set};

fn indexed_fill(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let obj = Gc::<()>::try_from(mt.regs()[last_index - 1]).unwrap_or_else(|_| {
        todo!() // error
    });
    let v = mt.regs()[last_index];

    let t = mt.borrow(obj.r#type());

    if !t.has_indexed {
        todo!() // Error: no indexed field
    }

    let field_descr = t.fields()[t.fields().len() - 1];
    for i in 0..unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) } {
        if !v.instance_of_dyn(mt, field_descr.r#type) {
            todo!("error: field type");
        }

        if !mt.borrow(field_descr.r#type).inlineable {
            unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef).add(i) = v; }
        } else {
            todo!()
        }
    }

    Answer::Ret {retc: 1} // HACK: happens to return `v`
}

pub const INDEXED_FILL: NativeFn = NativeFn {min_arity: 3, varargs: false, code: indexed_fill};

fn string_ref(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let s = mt.regs()[last_index - 1].try_cast::<String>(mt).unwrap_or_else(|| {
        todo!("not a string");
    });
    let i = isize::from(Fixnum::try_from(mt.regs()[last_index]).unwrap_or_else(|_| {
        todo!("index not a fixnum");
    }));
    let i = if i >= 0 {
        i as usize
    } else {
        todo!("negative index");
    };

    let c = mt.borrow(s).as_str().chars().nth(i).unwrap_or_else(|| {
        todo!("out of bounds");
    });

    mt.regs_mut()[last_index] = Char::from(c).into();
    Answer::Ret {retc: 1}
}

pub const STRING_REF: NativeFn = NativeFn {min_arity: 3, varargs: false, code: string_ref};

fn string_mut_ref(mt: &mut Mutator) -> Answer {
    let last_index = mt.regs().len() - 1;

    let s = mt.regs()[last_index - 1].try_cast::<StringMut>(mt).unwrap_or_else(|| {
        todo!("not a string");
    });
    let i = isize::from(Fixnum::try_from(mt.regs()[last_index]).unwrap_or_else(|_| {
        todo!("index not a fixnum");
    }));
    let i = if i >= 0 {
        i as usize
    } else {
        todo!("negative index");
    };

    let c = mt.borrow(s).as_str().chars().nth(i).unwrap_or_else(|| {
        todo!("out of bounds");
    });

    mt.regs_mut()[last_index] = Char::from(c).into();
    Answer::Ret {retc: 1}
}

pub const STRING_MUT_REF: NativeFn = NativeFn {min_arity: 3, varargs: false, code: string_mut_ref};

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
