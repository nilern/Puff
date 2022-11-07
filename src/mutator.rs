use std::ptr::NonNull;
use std::mem::{align_of, size_of, transmute};
use std::alloc::Layout;
use std::slice;

use crate::heap::{self, Heap};
use crate::oref::{ORef, Gc};
use crate::r#type::{Type, Field, IndexedType, NonIndexedType, BitsType, BootstrapTypeBuilder};
use crate::symbol::{Symbol, SymbolTable};
use crate::heap_obj::{NonIndexed, Indexed, Singleton, Header, min_size_of_indexed,
    align_of_indexed};
use crate::handle::{HandleAny, Handle, HandlePool, Root, root};
use crate::list::{EmptyList, Pair};
use crate::bytecode::{Opcode, Bytecode, decode_prune_mask};
use crate::vector::Vector;
use crate::closure::Closure;
use crate::regs::Regs;
use crate::r#box::Box;
use crate::namespace::{Namespace, Var};
use crate::native_fn::{self, NativeFn, Answer};
use crate::builtins;
use crate::bool::Bool;
use crate::fixnum::Fixnum;

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
            Header::initialize_indexed(type_nptr, Header::new(r#type.into()), Type::TYPE_LEN);
            type_nptr.as_ptr().write(IndexedType::new_unchecked(Type {
                min_size: min_size_of_indexed::<Type>(),
                align: align_of_indexed::<Type>(),
                is_bits: false,
                has_indexed: true,
                inlineable: false
            }));

            let field_type_nptr = heap.alloc_raw(Field::<()>::TYPE_LAYOUT, true)?.cast::<NonIndexedType>();
            let field_type = Gc::new_unchecked(field_type_nptr.cast::<NonIndexedType>());
            Header::initialize_indexed(field_type_nptr, Header::new(r#type.into()), Field::<()>::TYPE_LEN);
            field_type_nptr.as_ptr().write(NonIndexedType::new_unchecked(Type {
                min_size: size_of::<Field::<()>>(),
                align: align_of::<Field::<()>>(),
                is_bits: false,
                has_indexed: false,
                inlineable: true
            }));

            let usize_type_nptr = heap.alloc_raw(USIZE_TYPE_LAYOUT, true)?.cast::<BitsType>();
            let usize_type = Gc::new_unchecked(usize_type_nptr.cast::<BitsType>());
            Header::initialize_indexed(r#usize_type_nptr, Header::new(r#type.into()), 0);
            usize_type_nptr.as_ptr().write(BitsType::from_static::<usize>(true));

            let bool_nptr = heap.alloc_raw(BOOL_TYPE_LAYOUT, true)?.cast::<BitsType>();
            let bool = Gc::new_unchecked(bool_nptr.cast::<BitsType>());
            Header::initialize_indexed(r#bool_nptr, Header::new(r#type.into()), 0);
            bool_nptr.as_ptr().write(BitsType::from_static::<bool>(true));

            // Initialize Fields of strongly connected bootstrap types:
            // -----------------------------------------------------------------

            // `.min_size` etc. get reinitialized redundantly but that should not matter much:

            BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(r#type.into(), false)
                .field(usize_type.into(), false)
                .build(|len| {
                    debug_assert!(len == Field::<()>::TYPE_LEN);
                    Some(field_type_nptr)
                });

            BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(usize_type.into(), false)
                .field(usize_type.into(), false)
                .field(bool.into(), false)
                .field(bool.into(), false)
                .field(bool.into(), false)
                .indexed_field(field_type.into(), false)
                .build(|len| {
                    debug_assert!(len == Type::TYPE_LEN);
                    Some(type_nptr)
                });

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

            let u8_type = BootstrapTypeBuilder::<BitsType>::new::<u8>()
                .build(|| heap.alloc_indexed(r#type, 0).map(NonNull::cast))?;

            let symbol = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, false)
                .indexed_field(u8_type.into(), false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let string = BootstrapTypeBuilder::<NonIndexedType>::new()
                .indexed_field(u8_type.into(), false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let pair = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, true)
                .field(any, true)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let empty_list = BootstrapTypeBuilder::<NonIndexedType>::new()
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let vector_of_any = BootstrapTypeBuilder::<NonIndexedType>::new()
                .indexed_field(any, false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let vector_mut_of_any = BootstrapTypeBuilder::<NonIndexedType>::new()
                .indexed_field(any, true)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let pos = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, false)
                .field(any, false)
                .field(any, false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let syntax = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, false)
                .field(any, false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let bytecode = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(usize_type.into(), false)
                .field(bool.into(), false)
                .field(usize_type.into(), false)
                .field(usize_type.into(), false)
                .field(vector_of_any.into(), false)
                .field(vector_of_any.into(), false)
                .field(vector_of_any.into(), false)
                .indexed_field(u8_type.into(), false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let closure = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(bytecode.into(), false)
                .indexed_field(any, false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let fn_ptr = BootstrapTypeBuilder::<BitsType>::new::<native_fn::Code>()
                .build(|| heap.alloc_indexed(r#type, 0).map(NonNull::cast))?;

            let native_fn = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(usize_type.into(), false)
                .field(bool.into(), false)
                .field(fn_ptr.into(), false)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let r#box = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, true)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let namespace = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(usize_type.into(), true)
                .field(usize_type.into(), true)
                .field(vector_mut_of_any.into(), true)
                .field(vector_mut_of_any.into(), true)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

            let var = BootstrapTypeBuilder::<NonIndexedType>::new()
                .field(any, true)
                .build(|len| heap.alloc_indexed(r#type, len).map(NonNull::cast))?;

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

            let ns = root!(&mut mt, mt.ns.unwrap());
            for (name, f) in [("type-of", builtins::TYPE_OF), ("supertype", builtins::SUPERTYPE),
                ("field-get", builtins::FIELD_GET), ("eq?", builtins::EQ),
                ("fx+", builtins::FX_ADD), ("fx-", builtins::FX_SUB), ("fx*", builtins::FX_MUL),
                ("pair?", builtins::IS_PAIR), ("cons", builtins::CONS),
                ("set-car!", builtins::SET_CAR), ("set-cdr!", builtins::SET_CDR),
                ("eval-syntax", builtins::EVAL_SYNTAX), ("load", builtins::LOAD),
                ("apply", builtins::APPLY), ("values", builtins::VALUES)
            ] {
                let name = root!(&mut mt, Symbol::new(&mut mt, name));
                let f = root!(&mut mt, NativeFn::new(&mut mt, f));
                let var = root!(&mut mt, Var::new(&mut mt, f.borrow().into()));
                ns.clone().add(&mut mt, name.borrow(), var.borrow());
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
        self.consts = Some(self.borrow(code).consts);
        self.pc = 0;
    }

    unsafe fn code(&self) -> Gc<Bytecode> { self.code.unwrap() }

    unsafe fn consts(&self) -> Gc<Vector<ORef>> { self.consts.unwrap() }

    fn next_opcode(&mut self) -> Result<Opcode, ()> {
        let op = Opcode::try_from(self.borrow(unsafe { self.code() }).instrs()[self.pc]);
        self.pc += 1;
        op
    }

    fn peek_oparg(&self) -> usize { self.borrow(unsafe { self.code() }).instrs()[self.pc] as usize }

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

        if let Some(var) = self.borrow(self.ns.unwrap()).get(name) {
            self.regs.push(self.borrow(var).value());
        } else {
            todo!()
        }
    }

    pub fn pop(&mut self) -> Option<ORef> { self.regs.pop() }

    pub fn dump_regs(&self) { self.regs.dump(self); }

    pub unsafe fn alloc_nonindexed(&mut self, r#type: Gc<NonIndexedType>) -> NonNull<u8> {
        let type_hdl = root!(self, r#type);
        self.heap.alloc_nonindexed(r#type).unwrap_or_else(|| {
            self.collect();
            self.heap.alloc_nonindexed(type_hdl.oref()).expect("out of memory")
        })
    }

    pub unsafe fn alloc_indexed(&mut self, r#type: Gc<IndexedType>, len: usize) -> NonNull<u8> {
        let type_hdl = root!(self, r#type);
        self.heap.alloc_indexed(r#type, len).unwrap_or_else(|| {
            self.collect();
            self.heap.alloc_indexed(type_hdl.oref(), len).expect("out of memory")
        })
    }

    pub unsafe fn alloc_static<T: NonIndexed>(&mut self) -> NonNull<T> {
        self.alloc_nonindexed(T::reify_nonindexed(self)).cast::<T>()
    }

    pub fn root(&mut self, oref: ORef) -> HandleAny {
        unsafe { self.handles.root(oref) }
    }

    pub fn root_t<T>(&mut self, obj: Gc<T>) -> Handle<T> {
        unsafe { self.handles.root_t(obj) }
    }

    /// Deref `oref`. This is safe because allocation and thus GC is impossible while a `&Mutator` exists.
    /// Idea from the Josephine SpiderMonkey bindings (although I had the same intuition before hearing about that).
    pub fn borrow<'a, T>(&'a self, oref: Gc<T>) -> &'a T { unsafe { transmute(oref.as_ref()) } }

    unsafe fn mark_roots(&mut self) {
        self.handles.for_each_mut_root_freeing(|oref|
            self.heap.mark(transmute::<&mut ORef, *mut usize>(oref))
        );

        let types = slice::from_raw_parts_mut(
            transmute::<&mut Types, &mut Gc<Type>>(&mut self.types),
            size_of::<Types>() / size_of::<Gc<Type>>()
        );
        for r#type in types {
            self.heap.mark(transmute::<&mut Gc<Type>, *mut usize>(r#type));
        }

        let singletons = slice::from_raw_parts_mut(
            transmute::<&mut Singletons, &mut ORef>(&mut self.singletons),
            size_of::<Singletons>() / size_of::<ORef>()
        );
        for singleton in singletons {
            self.heap.mark(transmute::<&mut ORef, *mut usize>(singleton));
        }

        for ns in self.ns.iter_mut() {
            self.heap.mark(transmute::<&mut Gc<Namespace>, *mut usize>(ns));
        }

        for code in self.code.iter_mut() {
            self.heap.mark(transmute::<&mut Gc<Bytecode>, *mut usize>(code));
        }

        for consts in self.consts.iter_mut() {
            self.heap.mark(transmute::<&mut Gc<Vector<ORef>>, *mut usize>(consts));
        }

        for reg in self.regs.as_mut_slice().iter_mut() {
            self.heap.mark(transmute::<&mut ORef, *mut usize>(reg));
        }

        for slot in self.stack.iter_mut() {
            self.heap.mark(transmute::<&mut ORef, *mut usize>(slot));
        }
    }

    unsafe fn collect(&mut self) {
        if self.cfg.debug {
            println!("GC starting.");
        }

        self.heap.flip();

        self.mark_roots();

        self.heap.collect();

        self.symbols.scan();

        if self.cfg.debug {
            println!("GC finished.");

            self.heap.zero_fromspace();

            self.verify_objects().unwrap();

            println!("Heap verified.");
        }
    }

    unsafe fn verify_objects(&mut self) -> Result<(), heap::VerificationError> {
        self.verify_roots()?;
        self.heap.verify()
    }

    unsafe fn verify_roots(&self) -> Result<(), heap::VerificationError> {
        self.handles.verify(&self.heap)?;

        let types = slice::from_raw_parts(
            transmute::<&Types, &Gc<Type>>(&self.types),
            size_of::<Types>() / size_of::<Gc<Type>>()
        );
        for &r#type in types {
            self.heap.verify_oref(r#type.into())?;
        }

        let singletons = slice::from_raw_parts(
            transmute::<&Singletons, &ORef>(&self.singletons),
            size_of::<Singletons>() / size_of::<ORef>()
        );
        for &singleton in singletons {
            self.heap.verify_oref(singleton)?;
        }

        for &ns in self.ns.iter() {
            self.heap.verify_oref(ns.into())?;
        }

        for &code in self.code.iter() {
            self.heap.verify_oref(code.into())?;
        }

        for &consts in self.consts.iter() {
            self.heap.verify_oref(consts.into())?;
        }

        for &reg in self.regs.as_slice().iter() {
            self.heap.verify_oref(reg)?;
        }

        for &slot in self.stack.iter() {
            self.heap.verify_oref(slot)?;
        }

        self.symbols.verify(&self.heap)?;

        Ok(())
    }

    fn tailcall(&mut self, argc: usize) -> Option<Trampoline> {
        let callee = self.regs[self.regs.len() - argc];
        if let Some(callee) = callee.try_cast::<Closure>(self) {
            let code = self.borrow(callee).code;

            // Pass arguments:
            self.regs.enter(argc);

            // Jump:
            self.set_code(code);

            // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
            self.regs.ensure(self.borrow(code).max_regs);

            // TODO: GC safepoint (only becomes necessary with multithreading)

            // Check arity:
            let min_arity = unsafe { self.borrow(self.code()).min_arity };
            if ! unsafe { self.borrow(self.code()).varargs } {
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
            let min_arity = self.borrow(callee).min_arity;
            if !self.borrow(callee).varargs {
                if argc != min_arity {
                    todo!()
                }
            } else {
                if argc < min_arity {
                    todo!()
                }
            }

            // Call:
            Some(Trampoline::Answer((self.borrow(callee).code)(self)))
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
            let code = self.borrow(f).code;

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
                    self.regs.ensure(self.borrow(code).max_regs);

                    None
                },

                Opcode::IgnoreReturnValues => {
                    self.regs.re_enter(&self.stack[start..], 0);
                    self.stack.truncate(start);

                    // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                    self.regs.ensure(self.borrow(code).max_regs);

                    None
                },

                Opcode::TailCallWithValues => {
                    self.regs.re_enter(&self.stack[start..], retc);
                    self.stack.truncate(start);

                    // Ensure register space, reclaim garbage regs prefix and extend regs if necessary:
                    self.regs.ensure(self.borrow(code).max_regs);

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
                            unsafe { self.borrow(self.consts()).indexed_field()[i].unchecked_cast::<Symbol>() });
                        if let Some(var) = self.borrow(self.ns.unwrap()).get(name.oref()) {
                            let v = self.regs.pop().unwrap();
                            self.borrow(var).redefine(v);
                            unsafe { self.regs.push_unchecked(v); } // HACK?
                        } else {
                            unsafe {
                                let var = root!(self, Var::new_uninitialized(self));
                                root!(self, self.ns.unwrap()).add(self, name.borrow(), var.borrow());
                                let v = self.regs.pop().unwrap();
                                var.init(v); // Avoids allocating a HandleAny for `v`
                                self.regs.push_unchecked(v); // HACK?
                            }
                        }
                    }

                    Opcode::GlobalSet => {
                        let i = self.next_oparg();

                        let name = unsafe { self.borrow(self.consts()).indexed_field()[i].unchecked_cast::<Symbol>() };
                        if let Some(var) = self.borrow(self.ns.unwrap()).get(name) {
                            let v = self.regs.pop().unwrap();
                            self.borrow(var).set(v);
                            unsafe { self.regs.push_unchecked(v); } // HACK?
                        } else {
                            todo!()
                        }
                    }

                    Opcode::Global => {
                        let i = self.next_oparg();

                        let name = unsafe { self.borrow(self.consts()).indexed_field()[i].unchecked_cast::<Symbol>() };
                        if let Some(var) = self.borrow(self.ns.unwrap()).get(name) {
                            unsafe { self.regs.push_unchecked(self.borrow(var).value()); }
                        } else {
                            todo!("Unbound {}", self.borrow(name).name());
                        }
                    }

                    Opcode::Const => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.borrow(self.consts()).indexed_field()[i]); }
                    },

                    Opcode::Local => {
                        let i = self.next_oparg();

                        unsafe { self.regs.push_unchecked(self.regs[i]); }
                    },

                    Opcode::Clover => {
                        let i = self.next_oparg();

                        unsafe {
                            self.regs.push_unchecked(
                                self.borrow(self.regs[0].unchecked_cast::<Closure>()).clovers()[i]);
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

                        self.borrow(unsafe { r#box.unchecked_cast::<Box>() }).set(v);

                        self.regs.push(r#box); // FIXME: Abstraction leak wrt. `set!`-conversion
                    },

                    Opcode::CheckedBoxSet => {
                        let v = self.regs.pop().unwrap();
                        let r#box = self.regs.pop().unwrap();
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if self.borrow(guard).get().is_truthy(self) {
                            self.borrow(unsafe { r#box.unchecked_cast::<Box>() }).set(v);

                            self.regs.push(r#box); // FIXME: Abstraction leak wrt. `set!`-conversion
                        } else {
                            todo!()
                        }
                    },

                    Opcode::BoxGet => {
                        let r#box = self.regs.pop().unwrap();

                        self.regs.push(self.borrow(unsafe { r#box.unchecked_cast::<Box>() }).get());
                    },

                    Opcode::CheckedBoxGet => {
                        let r#box = self.regs.pop().unwrap();
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if self.borrow(guard).get().is_truthy(self) {
                            self.regs.push(self.borrow(unsafe { r#box.unchecked_cast::<Box>() }).get());
                        } else {
                            todo!()
                        }
                    },

                    Opcode::CheckUse => {
                        let guard = unsafe { self.regs.pop().unwrap().unchecked_cast::<Box>() };

                        if !self.borrow(guard).get().is_truthy(self) {
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

                        unsafe { self.regs.push_unchecked(self.borrow(self.consts()).indexed_field()[ci]); }
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
