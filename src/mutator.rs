use std::ptr::NonNull;
use std::mem::{align_of, size_of, transmute};
use std::alloc::Layout;
use std::slice;

use crate::heap::Heap;
use crate::oref::{AsType, ORef, Gc, Fixnum};
use crate::r#type::{Type, Field, IndexedType, NonIndexedType, BitsType};
use crate::symbol::{Symbol, SymbolTable};
use crate::heap_obj::{NonIndexed, Indexed, Singleton, Header, min_size_of_indexed,
    align_of_indexed};
use crate::handle::{Handle, HandleT, HandlePool, Root, root};
use crate::list::{EmptyList, Pair};
use crate::bytecode::{Opcode, Bytecode, decode_prune_mask};
use crate::vector::{Vector, VectorMut};
use crate::closure::Closure;
use crate::regs::Regs;
use crate::r#box::Box;
use crate::namespace::{Namespace, Var};
use crate::native_fn::{self, NativeFn, Answer};
use crate::builtins;
use crate::bool::Bool;
use crate::string::String;
use crate::syntax::{Syntax, Pos};

const USIZE_TYPE_SIZE: usize = min_size_of_indexed::<Type>();

const USIZE_TYPE_LAYOUT: Layout = unsafe {
    Layout::from_size_align_unchecked(
        USIZE_TYPE_SIZE, align_of_indexed::<Type>()
    )
};

const BOOL_TYPE_SIZE: usize = min_size_of_indexed::<Type>();

const BOOL_TYPE_LAYOUT: Layout = unsafe {
    Layout::from_size_align_unchecked(
        BOOL_TYPE_SIZE, align_of_indexed::<Type>()
    )
};

pub struct WithinMt<'a, T> {
    pub v: T,
    pub mt: &'a Mutator
}

pub struct Cfg {
    pub debug: bool
}

#[repr(C)]
pub struct Types {
    pub r#type: Gc<IndexedType>,
    pub bool: Gc<BitsType>,
    pub symbol: Gc<IndexedType>,
    pub string: Gc<IndexedType>,
    pub pair: Gc<NonIndexedType>,
    pub empty_list: Gc<NonIndexedType>,
    pub vector_of_any: Gc<IndexedType>,
    pub vector_mut_of_any: Gc<IndexedType>,
    pub pos: Gc<NonIndexedType>,
    pub syntax: Gc<NonIndexedType>,
    pub bytecode: Gc<IndexedType>,
    pub closure: Gc<IndexedType>,
    pub native_fn: Gc<NonIndexedType>,
    pub r#box: Gc<NonIndexedType>,
    pub namespace: Gc<NonIndexedType>,
    pub var: Gc<NonIndexedType>
}

#[repr(C)]
pub struct Singletons {
    pub r#true: Gc<Bool>,
    pub r#false: Gc<Bool>,
    pub empty_list: Gc<EmptyList>
}

pub struct Mutator {
    cfg: Cfg,

    heap: Heap,
    handles: HandlePool,

    types: Types,
    singletons: Singletons,
    symbols: SymbolTable,
    ns: Option<Gc<Namespace>>,

    code: Option<Gc<Bytecode>>,
    consts: Option<Gc<Vector<ORef>>>,
    pc: usize,
    regs: Regs<ORef>,
    stack: Vec<ORef>
}

enum Trampoline {
    Answer(Answer),
    Terminate {retc: usize}
}

impl Mutator {
    pub fn new(heap_size: usize, debug: bool) -> Option<Self> {
        let mut heap = Heap::new(heap_size);

        unsafe {
            // Create strongly connected bootstrap types, Fields uninitialized:
            // -----------------------------------------------------------------

            let type_nptr = heap.alloc_raw(Type::TYPE_LAYOUT, true)?.cast::<IndexedType>();
            let r#type = Gc::new_unchecked(type_nptr.cast::<IndexedType>());
            Header::initialize_indexed(type_nptr, Header::new(r#type.as_type()), Type::TYPE_LEN);
            type_nptr.as_ptr().write(IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Type>(),
                align: align_of_indexed::<Type>(),
                is_bits: false,
                has_indexed: true,
                inlineable: false
            }));

            let field_type_nptr = heap.alloc_raw(Field::<()>::TYPE_LAYOUT, true)?.cast::<NonIndexedType>();
            let field_type = Gc::new_unchecked(field_type_nptr.cast::<NonIndexedType>());
            Header::initialize_indexed(field_type_nptr, Header::new(r#type.as_type()), Field::<()>::TYPE_LEN);
            field_type_nptr.as_ptr().write(NonIndexedType::new_unchecked(Type {
                min_size: size_of::<Field::<()>>(),
                align: align_of::<Field::<()>>(),
                is_bits: false,
                has_indexed: false,
                inlineable: true
            }));

            let usize_type_nptr = heap.alloc_raw(USIZE_TYPE_LAYOUT, true)?.cast::<BitsType>();
            let usize_type = Gc::new_unchecked(usize_type_nptr.cast::<BitsType>());
            Header::initialize_indexed(r#usize_type_nptr, Header::new(r#type.as_type()), 0);
            usize_type_nptr.as_ptr().write(BitsType::from_static::<usize>(true));

            let bool_nptr = heap.alloc_raw(BOOL_TYPE_LAYOUT, true)?.cast::<BitsType>();
            let bool = Gc::new_unchecked(bool_nptr.cast::<BitsType>());
            Header::initialize_indexed(r#bool_nptr, Header::new(r#type.as_type()), 0);
            bool_nptr.as_ptr().write(BitsType::from_static::<bool>(true));

            // Initialize Fields of strongly connected bootstrap types:
            // -----------------------------------------------------------------

            {
                let fields = [
                    Field { r#type: r#type.as_type(), offset: 0 },
                    Field {
                        r#type: usize_type.as_type(),
                        offset: size_of::<Gc<()>>()
                    }
                ];
                (*field_type_nptr.cast::<Type>().as_ptr()).indexed_field_ptr_mut()
                    .copy_from_nonoverlapping(fields.as_ptr(), fields.len());
            }

            {
                let fields = [
                    Field { r#type: usize_type.as_type(), offset: 0 },
                    Field {
                        r#type: usize_type.as_type(),
                        offset: size_of::<usize>()
                    },
                    Field {
                        r#type: bool.as_type(),
                        offset: 2 * size_of::<usize>()
                    },
                    Field {
                        r#type: bool.as_type(),
                        offset: 2 * size_of::<usize>() + size_of::<bool>()
                    },
                    Field {
                        r#type: bool.as_type(),
                        offset: 2 * size_of::<usize>() + 2 * size_of::<bool>()
                    },
                    Field {
                        r#type: field_type.as_type(),
                        offset: min_size_of_indexed::<Type>()
                    }
                ];
                (*type_nptr.cast::<Type>().as_ptr()).indexed_field_ptr_mut()
                    .copy_from_nonoverlapping(fields.as_ptr(), fields.len());
            }

            // Create other `.types`:
            // -----------------------------------------------------------------

            let any = Type::bootstrap_new(&mut heap, r#type,
                Type {
                    min_size: size_of::<ORef>(),
                    align: align_of::<ORef>(),
                    is_bits: false,
                    has_indexed: false,
                    inlineable: false
                },
                &[])?;

            let u8_type = Type::bootstrap_new(&mut heap, r#type, BitsType::from_static::<u8>(true), &[])?;

            let symbol = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<Symbol>(),
                    align: align_of_indexed::<Symbol>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field { r#type: usize_type.as_type(), offset: 0 },
                  Field {
                      r#type: u8_type.as_type(),
                      offset: size_of::<usize>()
                  }])?;

            let string = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<String>(),
                    align: align_of_indexed::<String>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field {r#type: u8_type.as_type(), offset: min_size_of_indexed::<String>()}])?;

            let pair = Type::bootstrap_new(&mut heap, r#type, NonIndexedType::from_static::<Pair>(false, false), &[
                Field { r#type: any, offset: 0 },
                Field {
                    r#type: any,
                    offset: size_of::<Gc<()>>()
                }
            ])?;

            let empty_list = Type::bootstrap_new(&mut heap, r#type,
                NonIndexedType::from_static::<EmptyList>(false, false), &[])?;

            let vector_of_any = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<Vector::<ORef>>(),
                    align: align_of_indexed::<Vector::<ORef>>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field { r#type: any, offset: 0 }])?;

            let vector_mut_of_any = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<VectorMut::<ORef>>(),
                    align: align_of_indexed::<VectorMut::<ORef>>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field { r#type: any, offset: 0 }])?;

            let pos = Type::bootstrap_new(&mut heap, r#type, NonIndexedType::from_static::<Pos>(false, false), &[
                Field { r#type: any, offset: 0 },
                Field { r#type: any, offset: size_of::<ORef>() },
                Field { r#type: any, offset: 2 * size_of::<ORef>() }
            ])?;

            let syntax = Type::bootstrap_new(&mut heap, r#type, NonIndexedType::from_static::<Syntax>(false, false),
                &[Field { r#type: any, offset: 0 },
                  Field { r#type: any, offset: size_of::<ORef>() }])?;

            let bytecode = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<Bytecode>(),
                    align: align_of_indexed::<Bytecode>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field { r#type: usize_type.as_type(), offset: 0 },
                  Field { r#type: bool.as_type(), offset: size_of::<usize>() },
                  Field { r#type: usize_type.as_type(), offset: 2 * size_of::<usize>() },
                  Field { r#type: usize_type.as_type(), offset: 3 * size_of::<usize>() },
                  Field { r#type: vector_of_any.as_type(), offset: 4 * size_of::<usize>() },
                  Field { r#type: vector_of_any.as_type(), offset: 5 * size_of::<usize>() },
                  Field { r#type: vector_of_any.as_type(), offset: 6 * size_of::<usize>() },
                  Field {
                      r#type: u8_type.as_type(),
                      offset: min_size_of_indexed::<Bytecode>()
                  }])?;

            let closure = Type::bootstrap_new(&mut heap, r#type,
                IndexedType::new_unchecked(Type {
                    min_size: min_size_of_indexed::<Closure>(),
                    align: align_of_indexed::<Closure>(),
                    is_bits: false,
                    has_indexed: true,
                    inlineable: false
                }),
                &[Field { r#type: bytecode.as_type(), offset: 0 },
                  Field {
                      r#type: any,
                      offset: min_size_of_indexed::<Closure>()
                  }])?;

            let fn_ptr = Type::bootstrap_new(&mut heap, r#type, BitsType::from_static::<native_fn::Code>(true), &[])?;

            let native_fn = Type::bootstrap_new(&mut heap, r#type,
                NonIndexedType::from_static::<NativeFn>(false, false), &[
                Field { r#type: bytecode.as_type(), offset: 0 },
                Field { r#type: bool.as_type(), offset: size_of::<usize>() },
                Field { r#type: fn_ptr.as_type(), offset: 2* size_of::<usize>() }
            ])?;

            let r#box = Type::bootstrap_new(&mut heap, r#type, NonIndexedType::from_static::<Box>(false, false),
                &[Field { r#type: any, offset: 0 }])?;

            let namespace = Type::bootstrap_new(&mut heap, r#type,
                NonIndexedType::from_static::<Namespace>(false, false), &[
                    Field { r#type: usize_type.as_type(), offset: 0 },
                    Field { r#type: usize_type.as_type(), offset: size_of::<usize>() },
                    Field { r#type: vector_mut_of_any.as_type(), offset: 2 * size_of::<usize>() },
                    Field { r#type: vector_mut_of_any.as_type(), offset: 3 * size_of::<usize>() }
                ])?;

            let var = Type::bootstrap_new(&mut heap, r#type, NonIndexedType::from_static::<Var>(false, false),
                &[Field { r#type: any, offset: 0 }])?;

            // Create singleton instances:
            // -----------------------------------------------------------------

            let r#true = heap.alloc_nonindexed(bool.unchecked_cast())?.cast::<Bool>();
            r#true.as_ptr().write(Bool(true));
            let r#true = Gc::new_unchecked(r#true);

            let r#false = heap.alloc_nonindexed(bool.unchecked_cast())?.cast::<Bool>();
            r#false.as_ptr().write(Bool(false));
            let r#false = Gc::new_unchecked(r#false);

            let empty_list_inst = Gc::new_unchecked(heap.alloc_nonindexed(
                empty_list
            )?.cast::<EmptyList>());

            // -----------------------------------------------------------------

            let mut mt = Self {
                cfg: Cfg {debug},

                heap,
                handles: HandlePool::new(),

                types: Types { r#type, bool, symbol, string, pair, empty_list, pos, syntax, bytecode, vector_of_any,
                    vector_mut_of_any, closure, native_fn, r#box, namespace, var },
                singletons: Singletons { r#true, r#false, empty_list: empty_list_inst },
                symbols: SymbolTable::new(),
                ns: None,

                code: None,
                consts: None,
                pc: 0,
                regs: Regs::with_capacity((1 << 20 /* 1 MiB */) / size_of::<ORef>()),
                stack: Vec::new()
            };
            mt.ns = Some(Namespace::new(&mut mt));

            // Create builtins:
            // -----------------------------------------------------------------

            for (name, f) in [("eq?", builtins::EQ),
                ("fx+", builtins::FX_ADD), ("fx-", builtins::FX_SUB), ("fx*", builtins::FX_MUL),
                ("pair?", builtins::IS_PAIR), ("null?", builtins::IS_NULL), ("cons", builtins::CONS),
                ("car", builtins::CAR), ("cdr", builtins::CDR),
                ("set-car!", builtins::SET_CAR), ("set-cdr!", builtins::SET_CDR),
                ("eval-syntax", builtins::EVAL_SYNTAX), ("load", builtins::LOAD),
                ("apply", builtins::APPLY), ("values", builtins::VALUES)
            ] {
                let name = root!(&mut mt, Symbol::new(&mut mt, name));
                let f = root!(&mut mt, NativeFn::new(&mut mt, f));
                let var = root!(&mut mt, Var::new(&mut mt, f.into()));
                mt.ns.unwrap().as_ref().add(&mut mt, name, var);
            }

            // -----------------------------------------------------------------

            Some(mt)
        }
    }

    pub fn cfg(&self) -> &Cfg { &self.cfg }

    pub fn types(&self) -> &Types { &self.types }

    pub fn singletons(&self) -> &Singletons { &self.singletons }

    pub fn symbols(&self) -> &SymbolTable { &self.symbols }

    // HACK: returns raw pointer because of lifetime issues in Symbol::new:
    pub fn symbols_mut(&mut self) -> *mut SymbolTable { &mut self.symbols as _ }

    fn set_code(&mut self, code: Gc<Bytecode>) {
        self.code = Some(code);
        unsafe { self.consts = Some(code.as_ref().consts); }
        self.pc = 0;
    }

    unsafe fn code(&self) -> Gc<Bytecode> { self.code.unwrap() }

    unsafe fn consts(&self) -> Gc<Vector<ORef>> { self.consts.unwrap() }

    fn next_opcode(&mut self) -> Result<Opcode, ()> {
        let op = Opcode::try_from(unsafe { self.code().as_ref().instrs()[self.pc] });
        self.pc += 1;
        op
    }

    fn peek_oparg(&self) -> usize { unsafe { self.code().as_ref().instrs()[self.pc] as usize } }

    fn next_oparg(&mut self) -> usize {
        let arg = self.peek_oparg();
        self.pc += 1;
        arg
    }

    pub fn regs(&self) -> &Regs<ORef> { &self.regs }

    pub fn regs_mut(&mut self) -> &mut Regs<ORef> { &mut self.regs }

    pub fn push(&mut self, v: ORef) { self.regs.push(v); }

    pub fn push_global(&mut self, name: &str) {
        let name = Symbol::new(self, name);

        if let Some(var) = unsafe { self.ns.unwrap().as_ref().get(name) } {
            unsafe { self.regs.push(var.as_ref().value()); }
        } else {
            todo!()
        }
    }

    pub fn pop(&mut self) -> Option<ORef> { self.regs.pop() }

    pub fn dump_regs(&self) { self.regs.dump(self); }

    pub unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>) -> NonNull<u8> {
        self.heap.alloc_nonindexed(r#type).unwrap_or_else(|| {
            self.collect();
            self.heap.alloc_nonindexed(r#type).expect("out of memory")
        })
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize) -> NonNull<u8> {
        self.heap.alloc_indexed(r#type, len).unwrap_or_else(|| {
            self.collect();
            self.heap.alloc_indexed(r#type, len).expect("out of memory")
        })
    }

    pub unsafe fn alloc_static<T: NonIndexed>(&mut self) -> NonNull<T> {
        self.alloc_nonindexed(T::reify_nonindexed(self)).cast::<T>()
    }

    pub fn root(&mut self, oref: ORef) -> Handle {
        unsafe { self.handles.root(oref) }
    }

    pub fn root_t<T>(&mut self, obj: Gc<T>) -> HandleT<T> {
        unsafe { self.handles.root_t(obj) }
    }

    unsafe fn mark_roots(&mut self) {
        self.handles.for_each_root(|oref|
            if let Some(obj) = self.heap.mark(transmute::<ORef, usize>(*oref)) {
                *oref = obj.into();
            }
        );

        let types = slice::from_raw_parts_mut(
            transmute::<&mut Types, &mut Gc<Type>>(&mut self.types),
            size_of::<Types>() / size_of::<Gc<Type>>()
        );
        for r#type in types {
            if let Some(obj) = self.heap.mark(transmute::<Gc<Type>, usize>(*r#type)) {
                *r#type = obj.unchecked_cast::<Type>();
            }
        }

        let singletons = slice::from_raw_parts_mut(
            transmute::<&mut Singletons, &mut ORef>(&mut self.singletons),
            size_of::<Singletons>() / size_of::<ORef>()
        );
        for singleton in singletons {
            if let Some(obj) = self.heap.mark(transmute::<ORef, usize>(*singleton)) {
                *singleton = obj.into();
            }
        }

        for ns in self.ns.iter_mut() {
            if let Some(obj) = self.heap.mark(transmute::<Gc<Namespace>, usize>(*ns)) {
                *ns = obj.unchecked_cast::<Namespace>();
            }
        }

        for code in self.code.iter_mut() {
            if let Some(obj) = self.heap.mark(transmute::<Gc<Bytecode>, usize>(*code)) {
                *code = obj.unchecked_cast::<Bytecode>();
            }
        }

        for consts in self.consts.iter_mut() {
            if let Some(obj) = self.heap.mark(transmute::<Gc<Vector<ORef>>, usize>(*consts)) {
                *consts = obj.unchecked_cast::<Vector<ORef>>();
            }
        }

        for reg in self.regs.as_mut_slice().iter_mut() {
            if let Some(obj) = self.heap.mark(transmute::<ORef, usize>(*reg)) {
                *reg = obj.into();
            }
        }

        for slot in self.stack.iter_mut() {
            if let Some(obj) = self.heap.mark(transmute::<ORef, usize>(*slot)) {
                *slot = obj.into();
            }
        }
    }

    unsafe fn collect(&mut self) {
        self.heap.flip();

        self.mark_roots();

        todo!()
    }

    fn tailcall(&mut self, argc: usize) -> Option<Trampoline> {
        let callee = self.regs[self.regs.len() - argc];
        if let Some(callee) = callee.try_cast::<Closure>(self) {
            let code = unsafe { callee.as_ref().code };

            // Pass arguments:
            self.regs.enter(argc);

            // Jump:
            self.set_code(code);

            // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
            self.regs.ensure(unsafe { code.as_ref().max_regs });

            // TODO: GC safepoint (only becomes necessary with multithreading)

            // Check arity:
            let min_arity = unsafe { self.code().as_ref().min_arity };
            if ! unsafe { self.code().as_ref().varargs } {
                if argc != min_arity {
                    todo!()
                }
            } else {
                if argc < min_arity {
                    todo!()
                }

                let varargs_len = argc - min_arity;
                let acc_index = self.regs.len();
                self.regs.push(EmptyList::instance(self).into());
                for i in ((acc_index - varargs_len)..acc_index).rev() {
                    unsafe {
                        let pair = self.alloc_static::<Pair>();
                        pair.as_ptr().write(Pair::new(self.regs[i], self.regs[acc_index]));
                        self.regs[acc_index] = Gc::new_unchecked(pair).into();
                    }
                }

                unsafe { self.regs.popnnt_unchecked(varargs_len); }
            }

            None
        } else if let Some(callee) = callee.try_cast::<NativeFn>(self) {
            // Pass arguments:
            self.regs.enter(argc);

            // Check arity:
            let min_arity = unsafe { callee.as_ref().min_arity };
            if ! unsafe { callee.as_ref().varargs } {
                if argc != min_arity {
                    todo!()
                }
            } else {
                if argc < min_arity {
                    todo!()
                }
            }

            // Call:
            Some(Trampoline::Answer(unsafe { (callee.as_ref().code)(self) }))
        } else {
            todo!()
        }
    }

    fn ret(&mut self, retc: usize) -> Option<Trampoline> {
        if self.stack.len() > 0 {
            let rip =
                unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };
            let frame_len =
                unsafe { isize::from(Fixnum::from_oref_unchecked(self.stack.pop().unwrap())) as usize };

            let start = self.stack.len() - frame_len;
            let f = unsafe { self.stack[start].unchecked_cast::<Closure>() };
            let code = unsafe { f.as_ref().code };

            // Jump back:
            self.set_code(code);
            self.pc = rip;

            // Restore registers:
            match self.next_opcode().unwrap() {
                Opcode::CheckOneReturnValue => {
                    if retc != 1 {
                        todo!() // error
                    }

                    self.regs.re_enter(&self.stack[start..], retc);
                    self.stack.truncate(start);

                    // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                    self.regs.ensure(unsafe { code.as_ref().max_regs });

                    None
                },

                Opcode::IgnoreReturnValues => {
                    self.regs.re_enter(&self.stack[start..], 0);
                    self.stack.truncate(start);

                    // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                    self.regs.ensure(unsafe { code.as_ref().max_regs });

                    None
                },

                Opcode::TailCallWithValues => {
                    self.regs.re_enter(&self.stack[start..], retc);
                    self.stack.truncate(start);

                    // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                    self.regs.ensure(unsafe { code.as_ref().max_regs });

                    Some(Trampoline::Answer(Answer::TailCall {argc: retc + 1}))
                },

                _ => unreachable!()
            }
        } else {
            self.regs.enter(retc);

            Some(Trampoline::Terminate {retc})
        }
    }

    pub fn invoke(&mut self) -> &[ORef] {
        {
            let argc = self.regs.len();
            assert!(argc > 0);
            
            let mut trampoline = self.tailcall(argc);
            while let Some(tramp) = trampoline {
                trampoline = match tramp {
                    Trampoline::Answer(Answer::TailCall {argc}) => self.tailcall(argc),

                    Trampoline::Answer(Answer::Ret {retc}) => self.ret(retc),

                    Trampoline::Terminate {retc} => return &self.regs.as_slice()[self.regs.len() - retc..]
                };
            }
        }

        loop {
            if let Ok(op) = self.next_opcode() {
                match op {
                    Opcode::Define => {
                        let i = self.next_oparg();

                        let name = root!(self,
                            unsafe { self.consts().as_ref().indexed_field()[i].unchecked_cast::<Symbol>() });
                        if let Some(var) = unsafe { self.ns.unwrap().as_ref().get(*name) } {
                            let v = self.regs.pop().unwrap();
                            unsafe {
                                var.as_ref().redefine(v);
                                self.regs.push_unchecked(v); // HACK?
                            }
                        } else {
                            unsafe {
                                let var = root!(self, Var::new_uninitialized(self));
                                self.ns.unwrap().as_ref().add(self, name, var.clone());
                                let v = self.regs.pop().unwrap();
                                var.as_ref().init(v); // Avoids allocating a Handle for `v`
                                self.regs.push_unchecked(v); // HACK?
                            }
                        }
                    }

                    Opcode::GlobalSet => {
                        let i = self.next_oparg();

                        let name = unsafe { self.consts().as_ref().indexed_field()[i].unchecked_cast::<Symbol>() };
                        if let Some(var) = unsafe { self.ns.unwrap().as_ref().get(name) } {
                            let v = self.regs.pop().unwrap();
                            unsafe {
                                var.as_ref().set(v);
                                self.regs.push_unchecked(v); // HACK?
                            }
                        } else {
                            todo!()
                        }
                    }

                    Opcode::Global => {
                        let i = self.next_oparg();

                        let name = unsafe { self.consts().as_ref().indexed_field()[i].unchecked_cast::<Symbol>() };
                        if let Some(var) = unsafe { self.ns.unwrap().as_ref().get(name) } {
                            unsafe { self.regs.push_unchecked(var.as_ref().value()); }
                        } else {
                            unsafe { todo!("Unbound {}", name.as_ref().name()); }
                        }
                    }

                    Opcode::Const => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.consts().as_ref().indexed_field()[i]); }
                    },

                    Opcode::Local => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.regs[i]); }
                    },

                    Opcode::Clover => {
                        let i = self.next_oparg();

                        unsafe {
                            self.regs.push_unchecked(self.regs[0].unchecked_cast::<Closure>().as_ref().clovers()[i]);
                        }
                    },

                    Opcode::Pop => unsafe { self.regs.pop_unchecked(); },

                    Opcode::Prune => {
                        let regs_len = self.regs.len();
                        let mut reg = 0;
                        let mut mask_len = 0;
                        let mut free_reg = 0;
                        unsafe {
                            for prune in decode_prune_mask(&self.code().as_ref().instrs()[self.pc..]) {
                                if !prune && reg < regs_len {
                                    self.regs[free_reg] = self.regs[reg];
                                    free_reg += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }

                                reg += 1;
                            }

                            for reg in reg..regs_len {
                                self.regs[free_reg] = self.regs[reg];
                                free_reg += 1;
                            }
                        }
                        self.regs.truncate(free_reg);

                        self.pc += mask_len;
                    },

                    Opcode::Box => {
                        let last_index = self.regs.len() - 1;

                        let r#box = unsafe {
                            let nptr = self.alloc_static::<Box>();
                            nptr.as_ptr().write(Box::new(self.regs[last_index]));
                            Gc::new_unchecked(nptr)
                        };

                        self.regs[last_index] = r#box.into();
                    },

                    Opcode::UninitializedBox => {
                        let r#box = unsafe { Gc::new_unchecked(self.alloc_static::<Box>()) };

                        self.regs.push(r#box.into());
                    },

                    Opcode::BoxSet => {
                        let v = self.regs.pop().unwrap();
                        let r#box = self.regs.pop().unwrap();

                        unsafe { r#box.unchecked_cast::<Box>().as_ref().set(v); }

                        self.regs.push(r#box); // FIXME: Abstraction leak wrt. `set!`-conversion
                    },

                    Opcode::CheckedBoxSet => {
                        let v = self.regs.pop().unwrap();
                        let r#box = self.regs.pop().unwrap();
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if unsafe { guard.as_ref().get().is_truthy(self) } {
                            unsafe { r#box.unchecked_cast::<Box>().as_ref().set(v); }

                            self.regs.push(r#box); // FIXME: Abstraction leak wrt. `set!`-conversion
                        } else {
                            todo!()
                        }
                    },

                    Opcode::BoxGet => {
                        let r#box = self.regs.pop().unwrap();

                        self.regs.push(unsafe { r#box.unchecked_cast::<Box>().as_ref().get() });
                    },

                    Opcode::CheckedBoxGet => {
                        let r#box = self.regs.pop().unwrap();
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if unsafe { guard.as_ref().get().is_truthy(self) } {
                            self.regs.push(unsafe { r#box.unchecked_cast::<Box>().as_ref().get() });
                        } else {
                            todo!()
                        }
                    },

                    Opcode::CheckUse => {
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if !unsafe { guard.as_ref().get().is_truthy(self) } {
                            todo!()
                        }
                    },

                    Opcode::Brf =>
                        if self.regs.pop().unwrap().is_truthy(self) {
                            self.pc += 1;
                        } else {
                            let d = self.peek_oparg();

                            self.pc += d;
                        },

                    Opcode::Br => {
                        let d = self.peek_oparg();
                        
                        self.pc += d;
                    },

                    Opcode::Fn => {
                        let ci = self.next_oparg();
                        let len = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.consts().as_ref().indexed_field()[ci]); }
                        let closure = Closure::new(self, len);
                        unsafe { self.regs.popn_unchecked(len + 1); }
                        unsafe { self.regs.push_unchecked(closure.into()); }
                    },

                    Opcode::Call => {
                        let argc = self.next_oparg();

                        let max_frame_len = self.regs.len() - argc;
                        let mut reg = 0;
                        let mut frame_len = 0;
                        let mut mask_len = 0;
                        unsafe {
                            for prune in decode_prune_mask(&self.code().as_ref().instrs()[self.pc..]) {
                                if !prune && reg < max_frame_len {
                                    self.stack.push(self.regs[reg]);
                                    frame_len += 1;
                                }

                                if reg % 7 == 0 {
                                    mask_len += 1;
                                }

                                reg += 1;
                            }

                            for reg in reg..max_frame_len {
                                self.stack.push(self.regs[reg]);
                                frame_len += 1;
                            }
                        }

                        self.pc += mask_len;

                        self.stack.push(Fixnum::try_from(frame_len as isize).unwrap().into());
                        self.stack.push(Fixnum::try_from(self.pc as isize).unwrap().into());

                        let mut trampoline = self.tailcall(argc);
                        while let Some(tramp) = trampoline {
                            trampoline = match tramp {
                                Trampoline::Answer(Answer::TailCall {argc}) => self.tailcall(argc),

                                Trampoline::Answer(Answer::Ret {retc}) => self.ret(retc),

                                Trampoline::Terminate {retc} => return &self.regs.as_slice()[self.regs.len() - retc..]
                            };
                        }
                    },

                    Opcode::CheckOneReturnValue | Opcode::IgnoreReturnValues | Opcode::TailCallWithValues =>
                        unreachable!(),

                    Opcode::TailCall => {
                        let argc = self.peek_oparg();

                        let mut trampoline = self.tailcall(argc);
                        while let Some(tramp) = trampoline {
                            trampoline = match tramp {
                                Trampoline::Answer(Answer::TailCall {argc}) => self.tailcall(argc),

                                Trampoline::Answer(Answer::Ret {retc}) => self.ret(retc),

                                Trampoline::Terminate {retc} => return &self.regs.as_slice()[self.regs.len() - retc..]
                            };
                        }
                    },

                    Opcode::Ret => {
                        let mut trampoline = self.ret(1);
                        while let Some(tramp) = trampoline {
                            trampoline = match tramp {
                                Trampoline::Answer(Answer::TailCall {argc}) => self.tailcall(argc),

                                Trampoline::Answer(Answer::Ret {retc}) => self.ret(retc),

                                Trampoline::Terminate {retc} => return &self.regs.as_slice()[self.regs.len() - retc..]
                            };
                        }
                    }
                }
            } else {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mutator_new() {
        Mutator::new(1 << 20 /* 1 MiB */, false);
    }
}
