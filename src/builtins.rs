use std::fs;
use std::mem::size_of;
use std::slice;

use crate::heap_obj::{Header, Indexed};
use crate::native_fn::{self, Answer};
use crate::mutator::Mutator;
use crate::oref::{ReifyNontop, ORef, Gc};
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
use crate::vector::{Vector, VectorMut};
use crate::continuation::Continuation;
use crate::ports::{Port, Eof};

macro_rules! arity {
    ($mt:ident $(, $arg_ts:ty)*) => { 1 + arity!($($arg_ts),*) };
    ($arg_t:ty $(, $arg_ts:ty)*) => { 1 + arity!($($arg_ts),*) };
    () => { 0 };
}

macro_rules! builtin {
    {fn $name:ident, $make_param_types:ident, $fn_name:ident
        ($mt:ident $(, $arg_t:ty)*) $body:block
    } => {
        fn $make_param_types(mt: &mut Mutator) -> ORef {
            let ts = vec![$(root!(mt, <$arg_t>::reify_nontop(mt))),*];
            Vector::<ORef>::from_handles(mt, &ts).into()
        }

        fn $fn_name($mt: &mut Mutator) -> Answer $body

        pub const $name: native_fn::Builder = native_fn::Builder {
            min_arity: arity!($mt $(, $arg_t)*), varargs: false, make_param_types: $make_param_types,
            code: $fn_name
        };
    };

    {fn $name:ident, $make_param_types:ident, $fn_name:ident
        ($mt:ident $(, $arg_t:ty)*, ...) $body:block
    } => {
        fn $make_param_types(mt: &mut Mutator) -> ORef {
            let ts = vec![$(root!(mt, <$arg_t>::reify_nontop(mt))),*];
            Vector::<ORef>::from_handles(mt, &ts).into()
        }

        fn $fn_name($mt: &mut Mutator) -> Answer $body

        pub const $name: native_fn::Builder = native_fn::Builder {
            min_arity: arity!($mt $(, $arg_t)*) - 1, varargs: true, make_param_types: $make_param_types,
            code: $fn_name
        };
    };
}

fn any_param_types(mt: &mut Mutator) -> ORef { Bool::instance(mt, false).into() }

fn eq(mt: &mut Mutator) -> Answer {
    let a = mt.regs()[1];
    let b = mt.regs()[2];

    let res = a == b;

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
    Answer::Ret {retc: 1}
}

pub const EQ: native_fn::Builder = native_fn::Builder {
    min_arity: 3, varargs: false, make_param_types: any_param_types,
    code: eq
};

builtin!{
    fn FX_ADD, fx_add_params, fx_add(mt, Fixnum, Fixnum) {
        let a = unsafe { Fixnum::from_oref_unchecked(mt.regs()[1]) };
        let b = unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) };

        let res = match a.checked_add(b) {
            Some(res) => res,
            None => todo!("overflow")
        };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res.into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn FX_SUB, fx_sub_params, fx_sub(mt, Fixnum, Fixnum) {
        let a = unsafe { Fixnum::from_oref_unchecked(mt.regs()[1]) };
        let b = unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) };

        let res = match a.checked_sub(b) {
            Some(res) => res,
            None => todo!("overflow")
        };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res.into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn FX_MUL, fx_mul_params, fx_mul(mt, Fixnum, Fixnum) {
        let a = unsafe { Fixnum::from_oref_unchecked(mt.regs()[1]) };
        let b = unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) };

        let res = match a.checked_mul(b) {
            Some(res) => res,
            None => todo!("overflow")
        };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res.into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn FX_GT, fx_gt_params, fx_gt(mt, Fixnum, Fixnum) {
        let a = unsafe { Fixnum::from_oref_unchecked(mt.regs()[1]) };
        let b = unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = Bool::instance(mt, a > b).into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn CHAR_LENGTH_UTF8, char_length_utf8_params, char_length_utf8(mt, Char) {
        let c = char::from(unsafe { Char::from_oref_unchecked(mt.regs()[1]) });

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = Fixnum::from(c.len_utf8() as u8).into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn IS_INSTANCE, is_instance_params, is_instance(mt, Type, ORef) {
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() };
        let v = mt.regs()[2];

        let res = v.instance_of_dyn(mt, t);

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = Bool::instance(mt, res).into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn MAKE_ZEROED, make_zeroed_params, make_zeroed(mt, Type) {
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() };

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

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res;
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn MAKE_INDEXED_ZEROED, make_indexed_zeroed_params, make_indexed_zeroed(mt, Type, Fixnum) {
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() };
        let len = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let len = if len >= 0 {
            len as usize
        } else {
            todo!()
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

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res;
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn MAKE, make_params, make(mt, Type, ORef, ...) {
        let t = unsafe { mt.regs()[1].unchecked_cast::<Type>() };
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
}

builtin!{
    fn FIELD_REF, field_ref_params, field_ref(mt, ORef, Fixnum) {
        let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
            todo!() // error
        });
        let index = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
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

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = v;
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn FIELD_SET, field_set_params, field_set(mt, ORef, Fixnum, ORef) {
        let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
            todo!() // error
        });
        let index = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let index = if index >= 0 {
            index as usize
        } else {
            todo!() // error
        };
        let v = mt.regs()[3];

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
}

fn indexed_length(mt: &mut Mutator) -> Answer {
    let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
        todo!() // error
    });

    let t = mt.borrow(obj.r#type());

    if !t.has_indexed {
        todo!() // Error: no indexed field
    }

    let res = Fixnum::try_from(unsafe { *((obj.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) })
        .unwrap_or_else(|_| todo!() /* error */);

    let last_index = mt.regs().len() - 1;
    mt.regs_mut()[last_index] = res.into();
    Answer::Ret {retc: 1}
}

pub const INDEXED_LENGTH: native_fn::Builder = native_fn::Builder {
    min_arity: 2, varargs: false, make_param_types: any_param_types,
    code: indexed_length
};

builtin!{
    fn INDEXED_REF, indexed_ref_params, indexed_ref(mt, ORef, Fixnum) {
        let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
            todo!() // error
        });
        let index = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
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

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = v;
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn INDEXED_SET, indexed_set_params, indexed_set(mt, ORef, Fixnum, ORef) {
        let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
            todo!() // error
        });
        let index = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let index = if index >= 0 {
            index as usize
        } else {
            todo!() // error
        };
        let v = mt.regs()[3];

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
}

builtin!{
    fn INDEXED_COPY, indexed_copy_params, indexed_copy(mt, ORef, Fixnum, ORef, Fixnum, Fixnum) {
        let to = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|()| {
            todo!("to not a heap object");
        });
        let to_type = mt.borrow(to.r#type());
        if !to_type.has_indexed {
            todo!("to not indexed");
        }
        let to_len = unsafe { *((to.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) };

        let at = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
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

        let start = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[4]) });
        let start = if start >= 0 { start as usize } else { todo!("negative start"); };
        if start >= from_len { todo!("start out of bounds"); }

        let end = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[5]) });
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
}

fn indexed_fill(mt: &mut Mutator) -> Answer {
    let obj = Gc::<()>::try_from(mt.regs()[1]).unwrap_or_else(|_| {
        todo!() // error
    });
    let v = mt.regs()[2];

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

pub const INDEXED_FILL: native_fn::Builder = native_fn::Builder {
    min_arity: 3, varargs: false, make_param_types: any_param_types,
    code: indexed_fill
};

builtin!{
    fn INDEXED_CHAR_UTF8_SET, indexed_char_utf8_set_params, indexed_char_utf8_set(mt, ORef, Fixnum, Char) {
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

        let at = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let at = if at >= 0 { at as usize } else { todo!("negative at") };
        let bytes_length = unsafe { *((bytes.as_ptr() as *const Header).offset(-1) as *const usize).offset(-1) };
        if at >= bytes_length { todo!("at out of bounds"); }

        let c = char::from(unsafe { Char::from_oref_unchecked(mt.regs()[3]) });
        let c_len = c.len_utf8();
        if at + c_len > bytes_length { todo!("char does not fit"); }

        unsafe {
            let ptr = (bytes.as_ptr() as *mut u8).add(field_descr.offset).add(at);
            c.encode_utf8(slice::from_raw_parts_mut(ptr, c_len));
        }

        Answer::Ret {retc: 1} // HACK: happens to return `c`
    }
}


builtin!{
    fn STRING, string_params, string(mt, Char, ...) {
        let char_len = mt.regs().len() - 1;
        let mut byte_len = 0;
        for &c in &mt.regs().as_slice()[1..] {
            let c = char::from(unsafe { Char::from_oref_unchecked(c) });
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
}

builtin!{
    fn STRING_REF, string_ref_params, string_ref(mt, String, Fixnum) {
        let s = unsafe { mt.regs()[1].unchecked_cast::<String>() };
        let i = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let i = if i >= 0 {
            i as usize
        } else {
            todo!("negative index");
        };

        let c = mt.borrow(s).as_str().chars().nth(i).unwrap_or_else(|| {
            todo!("out of bounds");
        });

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = Char::from(c).into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn STRING_MUT_REF, string_mut_ref_params, string_mut_ref(mt, StringMut, Fixnum) {
        let s = unsafe { mt.regs()[1].unchecked_cast::<StringMut>() };
        let i = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let i = if i >= 0 {
            i as usize
        } else {
            todo!("negative index");
        };

        let c = mt.borrow(s).as_str().chars().nth(i).unwrap_or_else(|| {
            todo!("out of bounds");
        });

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = Char::from(c).into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn STRING_SET, string_set_params, string_set(mt, StringMut, Fixnum, Char) {
        let s = unsafe { mt.regs()[1].unchecked_cast::<StringMut>() };
        let i = isize::from(unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) });
        let i = if i >= 0 {
            i as usize
        } else {
            todo!("negative index");
        };
        let c = char::from(unsafe { Char::from_oref_unchecked(mt.regs()[3]) });

        root!(mt, s).borrow().set_nth(mt, i, c).unwrap_or_else(|_| {
            todo!("out of bounds");
        });

        Answer::Ret {retc: 1} // HACK: happens to return `c`
    }
}

builtin!{
    fn STRING_FILL, string_fill_params, string_fill(mt, StringMut, Char) {
        let s = root!(mt, unsafe { mt.regs()[1].unchecked_cast::<StringMut>() });
        let c = char::from(unsafe { Char::from_oref_unchecked(mt.regs()[2]) });

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
}

builtin!{
    fn EVAL_SYNTAX, eval_syntax_params, eval_syntax(mt, Syntax) {
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
}

builtin!{
    fn LOAD, load_params, load(mt, String) {
        let filename = root!(mt, unsafe { mt.regs()[mt.regs().len() - 1].unchecked_cast::<String>() });
        let s = fs::read_to_string(filename.as_str()).unwrap_or_else(|_| {
            todo!()
        });

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
}

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

pub const APPLY: native_fn::Builder = native_fn::Builder {
    min_arity: 3, varargs: true, make_param_types: any_param_types,
    code: apply 
};

fn values(mt: &mut Mutator) -> Answer {
    Answer::Ret {retc: mt.regs().len() - 1}
}

pub const VALUES: native_fn::Builder = native_fn::Builder {
    min_arity: 1, varargs: true, make_param_types: any_param_types,
    code: values
};

builtin!{
    fn CALL_CC, call_cc_params, call_cc(mt, ORef) {
        mt.regs_mut()[0] = mt.regs()[1];
        mt.regs_mut()[1] = Continuation::current(mt).into();

        Answer::TailCall {argc: 2}
    }
}

builtin!{
    fn CONTINUE, continue_params, r#continue(mt, Continuation, ORef, ...) {
        let k = unsafe { mt.regs()[1].unchecked_cast::<Continuation>() };

        mt.reinstate_continuation(k);

        Answer::Ret {retc: mt.regs().len() - 2}
    }
}

builtin!{
    fn OPEN_FILE, open_file_params, open_file(mt, String, Fixnum) {
        let filename = unsafe { root!(mt, mt.regs()[1].unchecked_cast::<String>()) };
        let oflag = unsafe { Fixnum::from_oref_unchecked(mt.regs()[2]) };

        let port = Port::open(mt, filename.borrow(), oflag);

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = port.into();
        Answer::Ret {retc: 1}
    }
}

builtin!{
    fn CLOSE_PORT, close_port_params, close_port(mt, Port) {
        let port = unsafe { mt.regs()[1].unchecked_cast::<Port>() };

        mt.borrow(port).close();

        Answer::Ret {retc: 1} // HACK: Happens to return `port`    
    }
}

builtin! {
    fn READ_CHAR, read_char_params, read_char(mt, Port) {
        let port = unsafe { mt.regs()[1].unchecked_cast::<Port>() };

        let res = match mt.borrow(port).read_char() {
            Some(c) => ORef::from(Char::from(c)),
            None => ORef::from(Eof::instance(mt))
        };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res;
        Answer::Ret {retc: 1}
    }
}

builtin! {
    fn PEEK_CHAR, peek_char_params, peek_char(mt, Port) {
        let port = unsafe { mt.regs()[1].unchecked_cast::<Port>() };

        let res = match mt.borrow(port).peek_char() {
            Some(c) => ORef::from(Char::from(c)),
            None => ORef::from(Eof::instance(mt))
        };

        let last_index = mt.regs().len() - 1;
        mt.regs_mut()[last_index] = res;
        Answer::Ret {retc: 1}
    }
}

builtin! {
    fn WRITE_CHAR, write_char_params, write_char(mt, Char, Port) {
        let c = unsafe { Char::from_oref_unchecked(mt.regs()[1]) };
        let port = unsafe { mt.regs()[2].unchecked_cast::<Port>() };

        mt.borrow(port).write_char(char::from(c));

        Answer::Ret {retc: 1} // HACK: happens to return `port`
    }
}
