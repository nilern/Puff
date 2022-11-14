use std::fs;
use std::mem::size_of;
use std::slice;

use crate::heap_obj::{Header, Indexed};
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
use crate::vector::VectorMut;
use crate::continuation::Continuation;
use crate::ports::{Port, Eof};

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

fn fx_gt(mt: &mut Mutator) -> Answer {
    let a = Fixnum::try_from(mt.regs()[1]).unwrap_or_else(|()| {
        todo!("non-fixnum a");
    });
    let b = Fixnum::try_from(mt.regs()[2]).unwrap_or_else(|()| {
        todo!("non-fixnum b");
    });

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = Bool::instance(mt, a > b).into();
    Answer::Ret {retc: 1}
}

pub const FX_GT: NativeFn = NativeFn {min_arity: 3, varargs: false, code: fx_gt};

fn char_length_utf8(mt: &mut Mutator) -> Answer {
    let c = char::from(Char::try_from(mt.regs()[1]).unwrap_or_else(|()| {
        todo!("not a char");
    }));

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = Fixnum::from(c.len_utf8() as u8).into();
    Answer::Ret {retc: 1}
}

pub const CHAR_LENGTH_UTF8: NativeFn = NativeFn {min_arity: 2, varargs: false, code: char_length_utf8};

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
        todo!() // error(fx+ 1 (call-with-current-continuation (lambda (k*)
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

fn indexed_copy(mt: &mut Mutator) -> Answer {
    let to = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|()| {
        todo!("to not a heap object");
    });
    let to_type = mt.borrow(to.r#type());
    if !to_type.has_indexed {
        todo!("to not indexed");
    }
    let to_len = unsafe { *((to.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) };

    let at = isize::from(Fixnum::try_from(mt.regs()[2]).unwrap_or_else(|_| {
        todo!("at not a fixnum");
    }));
    let at = if at >= 0 { at as usize } else { todo!("negative at"); };
    if at >= to_len { todo!("at out of bounds"); }

    let from = Gc::<()>::try_from(mt.regs()[3]).unwrap_or_else(|()| {
        todo!("from not a heap object");
    });
    let from_type = mt.borrow(from.r#type());
    if !from_type.has_indexed {
        todo!("from not indexed");
    }
    let from_len = unsafe { *((from.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) };

    let start = isize::from(Fixnum::try_from(mt.regs()[4]).unwrap_or_else(|_| {
        todo!("at not a fixnum");
    }));
    let start = if start >= 0 { start as usize } else { todo!("negative start"); };
    if start >= from_len { todo!("start out of bounds"); }

    let end = isize::from(Fixnum::try_from(mt.regs()[5]).unwrap_or_else(|_| {
        todo!("at not a fixnum");
    }));
    let end = if end >= 0 { end as usize } else { todo!("negative end"); };
    if end > from_len { todo!("end out of bounds"); }

    if start > end { todo!("start > end"); }

    let to_field_descr = to_type.fields()[to_type.fields().len() - 1];
    let from_field_descr = from_type.fields()[from_type.fields().len() - 1];

    if !from_field_descr.r#type.extends(mt, to_field_descr.r#type) {
        todo!()
    }

    let stride = if !mt.borrow(from_field_descr.r#type).inlineable {
        size_of::<ORef>()
    } else {
        unsafe { mt.borrow(to_field_descr.r#type.unchecked_cast::<NonIndexedType>()).stride() }
    };
    unsafe {
        let to_ptr = (to.as_ptr() as *mut u8).add(to_field_descr.offset).add(stride * at);
        let from_ptr = (from.as_ptr() as *const u8).add(from_field_descr.offset).add(stride * start);
        to_ptr.copy_from(from_ptr, stride * (end - start));
    }

    Answer::Ret {retc: 1} // HACK: happens to return `end`
}

pub const INDEXED_COPY: NativeFn = NativeFn {min_arity: 6, varargs: false, code: indexed_copy};

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
    if !v.instance_of_dyn(mt, field_descr.r#type) {
        todo!("error: field type");
    }
    
    if !mt.borrow(field_descr.r#type).inlineable {
        for i in 0..unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) } {
            unsafe { *((obj.as_ptr() as *mut u8).add(field_descr.offset) as *mut ORef).add(i) = v; }
        }
    } else {
        todo!()
    }

    Answer::Ret {retc: 1} // HACK: happens to return `v`
}

pub const INDEXED_FILL: NativeFn = NativeFn {min_arity: 3, varargs: false, code: indexed_fill};

fn indexed_char_utf8_set(mt: &mut Mutator) -> Answer {
    let bytes = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|()| {
        todo!("not a heap object");
    });
    let bytes_type = mt.borrow(bytes.r#type());
    if !bytes_type.has_indexed {
        todo!("nonindexed");
    }
    let field_descr = bytes_type.fields()[bytes_type.fields().len() - 1];
    if !mt.borrow(field_descr.r#type).inlineable {
        todo!("indexed field not bytes")
    }
    if !mt.borrow(field_descr.r#type).min_size == 1 {
        todo!("indexed field not bytes")
    }

    let at = isize::from(Fixnum::try_from(mt.regs()[2]).unwrap_or_else(|()| {
        todo!("at not a fixnum");
    }));
    let at = if at >= 0 { at as usize } else { todo!("negative at") };
    let bytes_length = unsafe { *((bytes.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) };
    if at >= bytes_length { todo!("at out of bounds"); }

    let c = char::from(Char::try_from(mt.regs()[3]).unwrap_or_else(|()| {
        todo!("not a char");
    }));
    let c_len = c.len_utf8();
    if at + c_len > bytes_length { todo!("char does not fit"); }

    unsafe {
        let ptr = (bytes.as_ptr() as *mut u8).add(field_descr.offset).add(at);
        c.encode_utf8(slice::from_raw_parts_mut(ptr, c_len));
    }

    Answer::Ret {retc: 1} // HACK: happens to return `c`
}

pub const INDEXED_CHAR_UTF8_SET: NativeFn = NativeFn {min_arity: 4, varargs: false, code: indexed_char_utf8_set};

fn string(mt: &mut Mutator) -> Answer {
    let char_len = mt.regs().len() - 1;
    let mut byte_len = 0;
    for &c in &mt.regs().as_slice()[1..] {
        let c = char::from(Char::try_from(c).unwrap_or_else(|()| {
            todo!("not a char");
        }));

        byte_len += c.len_utf8();
    }

    let bytes = root!(mt, VectorMut::<u8>::zeros(mt, byte_len));

    let byte_cells = bytes.indexed_field();
    let bytes_slice = unsafe { slice::from_raw_parts_mut(byte_cells.as_ptr() as *mut u8, byte_cells.len()) };
    let mut i = 0;
    for &c in &mt.regs().as_slice()[1..] {
        let c = char::from(unsafe { Char::from_oref_unchecked(c) });
        i += c.encode_utf8(&mut bytes_slice[i..]).len();
    }

    mt.regs_mut()[char_len] = StringMut::new(mt,
        Fixnum::try_from(char_len).unwrap(),
        Fixnum::try_from(byte_len).unwrap(),
        bytes.borrow()
    ).into();
    Answer::Ret {retc: 1}
}

pub const STRING: NativeFn = NativeFn {min_arity: 1, varargs: true, code: string};

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

fn string_set(mt: &mut Mutator) -> Answer {
    let s = mt.regs()[1].try_cast::<StringMut>(mt).unwrap_or_else(|| {
        todo!("not a string");
    });
    let i = isize::from(Fixnum::try_from(mt.regs()[2]).unwrap_or_else(|_| {
        todo!("index not a fixnum");
    }));
    let i = if i >= 0 {
        i as usize
    } else {
        todo!("negative index");
    };
    let c = char::from(Char::try_from(mt.regs()[3]).unwrap_or_else(|_| {
        todo!("not a char");
    }));

    root!(mt, s).borrow().set_nth(mt, i, c).unwrap_or_else(|_| {
        todo!("out of bounds");
    });

    Answer::Ret {retc: 1} // HACK: happens to return `c`
}

pub const STRING_SET: NativeFn = NativeFn {min_arity: 4, varargs: false, code: string_set};

fn string_fill(mt: &mut Mutator) -> Answer {
    let s = root!(mt, mt.regs()[1].try_cast::<StringMut>(mt).unwrap_or_else(|| {
        todo!("not a string");
    }));

    let c = char::from(Char::try_from(mt.regs()[2]).unwrap_or_else(|()| {
        todo!("not a char");
    }));

    let mut char_bytes = [0; 4];
    let char_bytes = c.encode_utf8(&mut char_bytes).as_bytes();
    let char_len = char_bytes.len();

    let string_char_len = isize::from(s.char_len) as usize; // `s.char_len` should always be >= 0
    let byte_len = char_len * string_char_len;

    let bytes = s.chars.get();
    let bytes = if mt.borrow(bytes).indexed_field().len() >= byte_len {
        bytes
    } else {
        VectorMut::<u8>::zeros(mt, byte_len)
    };
    
    let byte_cells = mt.borrow(bytes).indexed_field();
    let bytes_slice = unsafe { slice::from_raw_parts_mut(byte_cells.as_ptr() as *mut u8, byte_cells.len()) };
    let mut i = 0;
    for _ in 0..string_char_len {
        let end = i + char_len;
        bytes_slice[i..end].copy_from_slice(char_bytes);
        i = end;
    }

    s.byte_len.set(Fixnum::try_from(byte_len).unwrap());
    s.chars.set(bytes);

    Answer::Ret {retc: 1} // HACK: happens to return `c`
}

pub const STRING_FILL: NativeFn = NativeFn {min_arity: 3, varargs: false, code: string_fill};

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

fn call_cc(mt: &mut Mutator) -> Answer {
    mt.regs_mut()[0] = mt.regs()[1];
    mt.regs_mut()[1] = Continuation::current(mt).into();

    Answer::TailCall {argc: 2}
}

pub const CALL_CC: NativeFn = NativeFn {min_arity: 2, varargs: false, code: call_cc};

fn r#continue(mt: &mut Mutator) -> Answer {
    let k = mt.regs()[1].try_cast::<Continuation>(mt).unwrap_or_else(|| {
        todo!("not a continuation")
    });

    mt.reinstate_continuation(k);

    Answer::Ret {retc: mt.regs().len() - 2}
}

pub const CONTINUE: NativeFn = NativeFn {min_arity: 2, varargs: true, code: r#continue};

fn open_file(mt: &mut Mutator) -> Answer {
    let filename = root!(mt, mt.regs()[1].try_cast::<String>(mt).unwrap_or_else(|| {
        todo!("not a string");
    }));
    let oflag = Fixnum::try_from(mt.regs()[2]).unwrap_or_else(|()| {
        todo!("not a fixnum");
    });

    let port = Port::open(mt, filename.borrow(), oflag);

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = port.into();
    Answer::Ret {retc: 1}
}

pub const OPEN_FILE: NativeFn = NativeFn {min_arity: 3, varargs: false, code: open_file};

fn read_char(mt: &mut Mutator) -> Answer {
    let port = mt.regs()[1].try_cast::<Port>(mt).unwrap_or_else(|| {
        todo!("not a port");
    });

    let res = match mt.borrow(port).read_char() {
        Some(c) => ORef::from(Char::from(c)),
        None => ORef::from(Eof::instance(mt))
    };

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = res;
    Answer::Ret {retc: 1}
}

pub const READ_CHAR: NativeFn = NativeFn {min_arity: 2, varargs: false, code: read_char};
