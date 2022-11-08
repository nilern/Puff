use std::mem::{transmute, size_of, align_of};
use std::alloc::Layout;
use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::heap::Heap;
use crate::oref::{Reify, Gc, ORef};
use crate::heap_obj::{HeapObj, Indexed, min_size_of_indexed, align_of_indexed,
    item_stride};
use crate::mutator::Mutator;

#[repr(C)]
pub struct Field<T> {
    pub r#type: Gc<T>,
    pub offset: usize
}

impl<T> Clone for Field<T> {
    fn clone(&self) -> Self {
        Self {
            r#type: self.r#type,
            offset: self.offset
        }
    }
}

impl<T> Copy for Field<T> {}

impl<T> Field<T> {
    pub const TYPE_LEN: usize = 2;

    pub const TYPE_SIZE: usize = min_size_of_indexed::<Type>()
        + Self::TYPE_LEN * item_stride::<Type>();

    pub const TYPE_LAYOUT: Layout = unsafe {
        Layout::from_size_align_unchecked(
            Self::TYPE_SIZE, align_of_indexed::<Type>()
        )
    };
}

#[repr(C)]
pub struct Type {
    pub align: usize,
    pub min_size: usize,
    pub is_bits: bool,
    pub has_indexed: bool,
    pub inlineable: bool,
}

unsafe impl HeapObj for Type {}

unsafe impl Indexed for Type {
    type Item = Field<Type>;
}

impl Reify for Type {
    type Kind = IndexedType;

    fn reify(mt: &Mutator) -> Gc<Self::Kind> { mt.types().r#type }
}

impl Type {
    pub const TYPE_LEN: usize = 6;

    pub const TYPE_SIZE: usize = min_size_of_indexed::<Type>()
        + Self::TYPE_LEN * item_stride::<Type>();

    pub const TYPE_LAYOUT: Layout = unsafe {
        Layout::from_size_align_unchecked(
            Self::TYPE_SIZE, align_of_indexed::<Type>()
        )
    };

    pub unsafe fn bootstrap_new<T>(heap: &mut Heap, type_type: Gc<IndexedType>, fixed: T, fields: &[Field<Type>])
        -> Option<Gc<T>>
    {
        heap.alloc_indexed(type_type, fields.len()).map(|nptr| {
            let nptr = nptr.cast::<T>();
            nptr.as_ptr().write(fixed);
            (*nptr.cast::<Type>().as_ptr()).indexed_field_ptr_mut()
                .copy_from_nonoverlapping(fields.as_ptr(), fields.len());
            Gc::new_unchecked(nptr)
        })
    }

    pub fn fields(&self) -> &[Field<Type>] { self.indexed_field() }
}

impl Gc<Type> {
    pub fn extends(self, mt: &Mutator, sup: Gc<Type>) -> bool {
        // TODO: follow supertype chain
        self == sup
        || sup == mt.types().any // HACK
    }
}

#[repr(C)]
pub struct NonIndexedType(Type);

unsafe impl HeapObj for NonIndexedType {}

unsafe impl Indexed for NonIndexedType {
    type Item = Field<Type>;
}

impl From<Gc<NonIndexedType>> for Gc<Type> {
    fn from(t: Gc<NonIndexedType>) -> Self { unsafe { t.unchecked_cast::<Type>() } }
}

impl NonIndexedType {
    pub fn new_unchecked(r#type: Type) -> Self { Self(r#type) }

    pub fn align(&self) -> usize { self.0.align }

    pub fn min_size(&self) -> usize { self.0.min_size }

    pub fn stride(&self) -> usize { (self.min_size() + self.align() - 1) & !(self.align() - 1) }

    pub fn layout(&self) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(self.min_size(), self.align())
        }
    }
}

#[repr(C)]
pub struct BitsType(Type);

unsafe impl HeapObj for BitsType {}

impl From<Gc<BitsType>> for Gc<Type> {
    fn from(t: Gc<BitsType>) -> Self { unsafe { t.unchecked_cast::<Type>() } }
}

impl BitsType {
    pub fn from_static<T>(inlineable: bool) -> Self {
        Self(Type {
            align: align_of::<T>(),
            min_size: size_of::<T>(),
            is_bits: true,
            has_indexed: false,
            inlineable
        })
    }
}

#[repr(C)]
pub struct IndexedType(Type);

unsafe impl HeapObj for IndexedType {}

unsafe impl Indexed for IndexedType {
    type Item = Field<Type>;
}

impl TryFrom<Gc<Type>> for Gc<IndexedType> {
    type Error = ();

    fn try_from(r#type: Gc<Type>) -> Result<Self, Self::Error> {
        if unsafe { r#type.as_ref().has_indexed } {
            Ok(unsafe { r#type.unchecked_cast::<IndexedType>() })
        } else {
            Err(())
        }
    }
}

impl From<Gc<IndexedType>> for Gc<Type> {
    fn from(t: Gc<IndexedType>) -> Self { unsafe { t.unchecked_cast::<Type>() } }
}

impl IndexedType {
    pub fn new_unchecked(r#type: Type) -> Self { Self(r#type) }

    fn align(&self) -> usize { self.0.align }

    fn min_size(&self) -> usize { self.0.min_size }

    fn indexed_field(&self) -> &Field<NonIndexedType> {
        let fields = self.0.fields();
        unsafe { transmute::<&Field<Type>, &Field<NonIndexedType>>(
            &fields[fields.len() - 1]
        ) }
    }

    pub fn size(&self, len: usize) -> usize {
        let item_stride = unsafe { self.indexed_field().r#type.as_ref().stride() };
        self.min_size() + len * item_stride
    }

    pub fn layout(&self, len: usize) -> Layout {
        unsafe { Layout::from_size_align_unchecked(self.size(len), self.align()) }
   }
}

pub struct BootstrapTypeBuilder<T> {
    align: usize,
    min_size: usize,
    is_bits: bool,
    has_indexed: bool,
    inlineable: bool,
    fields: Vec<Field<Type>>,
    phantom: PhantomData<T>
}

impl BootstrapTypeBuilder<BitsType> {
    pub fn new<T>() -> Self {
        BootstrapTypeBuilder {
            align: align_of::<T>(),
            min_size: size_of::<T>(),
            is_bits: true,
            has_indexed: false,
            inlineable: true,
            fields: Vec::new(),
            phantom: PhantomData::default()
        }
    }
}

impl BootstrapTypeBuilder<NonIndexedType> {
    pub fn new() -> Self {
        BootstrapTypeBuilder {
            align: 1,
            min_size: 0,
            is_bits: false,
            has_indexed: false,
            inlineable: true,
            fields: Vec::new(),
            phantom: PhantomData::default()
        }
    }
}

impl BootstrapTypeBuilder<NonIndexedType> {
    pub unsafe fn field(mut self, r#type: Gc<Type>, mutable: bool) -> Self {
        let field_type = r#type.as_ref();

        let (field_align, field_size) = if !field_type.inlineable {
            (align_of::<ORef>(), size_of::<ORef>())
        } else {
            (field_type.align, field_type.min_size)
        };
        let offset = (self.min_size + field_align - 1) & !(field_align - 1);

        self.align = self.align.max(field_align);
        self.min_size = offset + field_size;
        self.fields.push(Field {r#type, offset});
        self.inlineable = self.inlineable && !mutable;

        self
    }
    
    pub unsafe fn indexed_field(mut self, r#type: Gc<Type>, mutable: bool) -> BootstrapTypeBuilder<IndexedType> {
        let field_type = r#type.as_ref();

        let field_align = if !field_type.inlineable { align_of::<ORef>() } else { field_type.align };
        let offset = (self.min_size + field_align - 1) & !(field_align - 1);

        self.fields.push(Field {r#type, offset});

        BootstrapTypeBuilder {
            align: self.align.max(field_align),
            min_size: offset,
            is_bits: self.is_bits,
            has_indexed: true,
            inlineable: false,
            fields: self.fields,
            phantom: PhantomData::default()
        }
    }
}

impl<T: Indexed<Item=Field<Type>>> BootstrapTypeBuilder<T> {
    pub unsafe fn build<F: FnOnce(usize) -> Option<NonNull<T>>>(self, alloc_type: F) -> Option<Gc<T>> {
        alloc_type(self.fields.len()).map(|mut t| {
            t.as_ptr().cast::<Type>().write(Type {
                align: self.align,
                min_size: self.min_size,
                is_bits: self.is_bits,
                has_indexed: self.has_indexed,
                inlineable: self.inlineable
            });

            let mut dest_field = t.as_mut().indexed_field_ptr_mut();
            for field in self.fields {
                dest_field.write(field);
                dest_field = dest_field.add(1);
            }

            Gc::new_unchecked(t)
        })
    }
}

impl BootstrapTypeBuilder<BitsType> {
    pub unsafe fn build<F: FnOnce() -> Option<NonNull<BitsType>>>(self, alloc_type: F) -> Option<Gc<BitsType>> {
        alloc_type().map(|t| {
            t.as_ptr().cast::<Type>().write(Type {
                align: self.align,
                min_size: self.min_size,
                is_bits: self.is_bits,
                has_indexed: self.has_indexed,
                inlineable: self.inlineable
            });

            Gc::new_unchecked(t)
        })
    }
}
