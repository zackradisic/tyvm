use std::{
    alloc::{alloc, Layout},
    borrow::Cow,
    ffi::c_void,
    fmt::{self, Write},
    ptr,
};

#[derive(Clone, Copy, Debug)]
pub struct ObjectField(pub *mut StringRef, pub Value);
impl ObjectField {
    pub fn key_as_slice(&self) -> &str {
        unsafe { (*self.0).as_slice() }
    }
}

impl From<(*mut StringRef, Value)> for ObjectField {
    fn from((str_ref, val): (*mut StringRef, Value)) -> Self {
        Self(str_ref, val)
    }
}

#[derive(Clone)]
pub struct Object {
    /// INVARIANT: must be lexographically ordered so binary search works
    pub fields: Vec<ObjectField>,
}

impl Object {
    pub fn alloc(self) -> *mut Object {
        let layout = Layout::for_value(&self).align_to(16).unwrap();
        unsafe {
            let bytes = alloc(layout) as *mut _;
            *bytes = self;
            bytes
        }
    }
}

impl std::fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct("Object");
        for ObjectField(key, val) in self.fields.iter() {
            let str_key = unsafe { key.as_ref().unwrap().to_string() };
            // unsafe {
            //     let k = key.as_ref().unwrap();
            //     let str_key = k.as_slice();
            f.field(&str_key, &val);
            // }
        }
        f.finish()
    }
}

impl<A: Into<ObjectField>> FromIterator<A> for Object {
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        let mut fields = iter.into_iter().map(|a| a.into()).collect::<Vec<_>>();
        fields.sort_by(|a, b| a.key_as_slice().cmp(b.key_as_slice()));
        Self { fields }
    }
}

#[derive(Clone, Copy)]
pub struct HeapString {
    pub ptr: *mut u8,
    pub len: usize,
}

impl HeapString {
    pub fn to_string_ref(self) -> StringRef {
        unsafe { std::mem::transmute(self) }
    }

    pub fn as_slice<'a>(self) -> &'a str {
        unsafe {
            let bytes = std::slice::from_raw_parts(self.ptr, self.len);
            std::str::from_utf8_unchecked(bytes)
        }
    }

    pub fn to_string(self) -> String {
        self.as_slice().to_string()
    }
}

impl std::fmt::Debug for HeapString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.debug_tuple("HeapString").field(&self.as_slice()).finish()
        f.debug_tuple("HeapString")
            .field(&self.as_slice())
            // .field(&self.to_string())
            .finish()
    }
}

#[derive(Copy, Clone)]
pub struct InlinedString {
    pub tag_and_len: u8,
    pub chars: [u8; 15],
}

impl InlinedString {
    pub fn len(self) -> u8 {
        (self.tag_and_len & 0b11111110) >> 1
    }

    pub fn as_slice(&self) -> &str {
        let len = self.len();
        unsafe { std::str::from_utf8_unchecked(&self.chars.as_slice()[0..len as usize]) }
    }

    pub fn to_string_ref(self) -> StringRef {
        unsafe { std::mem::transmute(self) }
    }

    pub fn to_string(self) -> String {
        self.as_slice().to_string()
    }
}

impl std::fmt::Debug for InlinedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("InlinedString")
            .field(&self.as_slice())
            // .field(&self.to_string())
            .finish()
    }
}

#[derive(Copy, Clone)]
#[repr(transparent)]
pub struct StringRef(u128);

impl StringRef {
    pub fn is_inlined(self) -> bool {
        self.0 & 1 == 1
    }

    pub fn is_heap_allocated(self) -> bool {
        !self.is_inlined()
    }

    pub fn to_string(self) -> String {
        if self.is_inlined() {
            self.as_inlined().as_slice().to_owned()
        } else {
            self.as_heap().as_slice().to_owned()
        }
    }

    pub fn as_str<'a>(&'a self) -> Cow<'a, str> {
        if self.is_inlined() {
            self.as_inlined().to_string().into()
        } else {
            self.as_heap().as_slice().into()
        }
    }

    pub fn as_slice<'a>(&'a self) -> &'a str {
        if self.is_inlined() {
            self.as_inlined_ref().as_slice()
        } else {
            self.as_heap().as_slice()
        }
    }

    pub fn as_inlined(self) -> InlinedString {
        unsafe { std::mem::transmute(self) }
    }

    pub fn as_inlined_ref(&self) -> &InlinedString {
        unsafe { std::mem::transmute(self) }
    }

    pub fn as_heap(self) -> HeapString {
        unsafe { std::mem::transmute(self) }
    }

    pub fn new(string: &str) -> Self {
        if string.len() <= 15 {
            Self::new_inlined(string)
        } else {
            Self::alloc_new_heap(string)
        }
    }

    pub fn new_inlined(chars: &str) -> Self {
        assert!(chars.len() <= 15);
        let len_u8 = chars.len() as u8;
        let mut chars_buf = [0u8; 15];
        (&mut chars_buf[0..chars.len()]).copy_from_slice(chars.as_bytes());
        // let chars: [u8; 15] = chars.as_bytes().try_into().unwrap();
        InlinedString {
            tag_and_len: (len_u8 << 1) | 0b00000001,
            chars: chars_buf,
        }
        .to_string_ref()
    }

    pub fn alloc_new_heap(string: &str) -> Self {
        let layout = Layout::for_value(string.as_bytes()).align_to(16).unwrap();
        let chars = unsafe {
            let ptr = alloc(layout);
            std::ptr::copy_nonoverlapping(string.as_bytes().as_ptr(), ptr, string.as_bytes().len());
            ptr
        };
        StringRef::new_heap(chars, string.as_bytes().len())
    }

    pub fn new_heap(ptr: *mut u8, len: usize) -> Self {
        HeapString { ptr, len }.to_string_ref()
    }
}

impl std::fmt::Debug for StringRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_inlined() {
            f.debug_tuple("StringRef")
                .field(&self.as_inlined())
                .finish()
        } else {
            f.debug_tuple("StringRef").field(&self.as_heap()).finish()
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ObjRef(*mut c_void);
impl ObjRef {
    /// 0b0000
    pub const TAG_STR: usize = 1;
    /// 0b0001
    pub const TAG_OBJ: usize = 2;
    /// 0b1111
    pub const TAG_MASK: usize = 15;

    pub fn from_obj(obj: *mut Object) -> Self {
        Self(((obj as usize) | Self::TAG_OBJ) as *mut _)
    }

    pub fn is_obj(self) -> bool {
        self.as_usize() & Self::TAG_OBJ == Self::TAG_OBJ
    }

    pub fn as_obj_ref(&self) -> &Object {
        #[cfg(feature = "safe_obj")]
        assert!(self.is_obj());
        unsafe { self.as_obj().as_ref().unwrap_unchecked() }
    }

    pub fn as_obj(self) -> *mut Object {
        #[cfg(feature = "safe_obj")]
        assert!(self.is_obj());

        (self.as_usize() & !Self::TAG_MASK) as *mut Object
    }

    pub fn alloc_new_str_ref(str_ref: StringRef) -> Self {
        unsafe {
            let layout =
                Layout::from_size_align_unchecked(std::mem::size_of::<*mut StringRef>(), 16);
            let mut ptr = alloc(layout) as *mut StringRef;
            *ptr = str_ref;
            Self::from_str_ref(ptr)
        }
    }

    pub fn from_str_ref(str_ref: *mut StringRef) -> Self {
        Self(((str_ref as usize) | Self::TAG_STR) as *mut _)
    }

    pub fn is_str_ref(self) -> bool {
        self.as_usize() & Self::TAG_STR == Self::TAG_STR
    }

    pub fn as_str_ref(self) -> *mut StringRef {
        #[cfg(feature = "safe_obj")]
        assert!(self.is_str_ref());

        (self.as_usize() & !Self::TAG_MASK) as *mut StringRef
    }

    pub fn as_str_ref_owned(self) -> StringRef {
        #[cfg(feature = "safe_obj")]
        {
            assert!(self.is_str_ref())
        }
        unsafe { *self.as_str_ref() }
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }

    pub fn as_u64(self) -> u64 {
        self.as_usize() as u64
    }

    pub fn from_usize(ptr: usize) -> Self {
        Self(ptr as *mut c_void)
    }
}

impl std::fmt::Debug for ObjRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_str_ref() {
            f.debug_tuple("ObjRef")
                .field(&self.as_str_ref_owned())
                .finish()
        } else {
            f.debug_tuple("ObjRef").field(&self.as_obj_ref()).finish()
        }
    }
}

impl From<*mut Object> for ObjRef {
    fn from(value: *mut Object) -> Self {
        Self::from_obj(value)
    }
}

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum KeywordType {
    Null = 1,
    Undefined,
    Void,
    Never,

    String,
    Number,
    Bool,
    Object,

    True,
    False,
}

impl KeywordType {
    const fn as_u8(self) -> u8 {
        self as u8
    }

    const fn as_u64(self) -> u64 {
        self.as_u8() as u64
    }

    const fn from_bool(b: bool) -> Self {
        if b {
            Self::True
        } else {
            Self::False
        }
    }
}

/// # Nanboxed value
/// Slightly modified nanbox implementation.
///
/// Type-level Typescript has no way to express a NaN in the type-system, so we
/// use a NaN bit pattern to indicate the value can be an object, boolean, etc.
///
/// We require that pointers are all 16-byte aligned so
/// there is an additional 4 bottom bits available to use.
///
///    NN NN NN NN NN NN NN NN NN NN NN XX XX XX XX PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP PP XX XX XX XX       
/// __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __ __
/// 63 62 61 60 59 58 57 56 55 54 53 52 51 50 49 48 47 46 45 44 43 42 41 40 39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
#[derive(Copy, Clone)]
pub struct Value(u64);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_keyword_type() {
            return f
                .debug_tuple("Value")
                .field(&self.as_keyword_type())
                .finish();
        }
        if self.is_num() {
            return f.debug_tuple("Value").field(&self.as_num()).finish();
        }
        if self.is_obj() {
            return f.debug_tuple("Value").field(&self.as_obj_ref()).finish();
        }
        f.debug_tuple("Value").field(&"WTF").finish()
    }
}

impl Value {
    // all exponent bits set to 1
    const NAN: u64 = 0x7ff8000000000000;
    /// 0b0001
    const TAG_KEYWORD_TYPE: u64 = 1 << 48;
    /// 0b1111
    const TAG_MASK: u64 = 15 << 48;

    pub const NULL: Value = Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Null.as_u64());
    pub const UNDEFINED: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Undefined.as_u64());
    pub const VOID: Value = Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Void.as_u64());
    pub const NEVER: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Never.as_u64());
    pub const NUMBER_KW: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Number.as_u64());
    pub const STRING_KW: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::String.as_u64());
    pub const OBJECT_KW: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Object.as_u64());
    pub const BOOL_KW: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::Bool.as_u64());

    pub const TRUE: Value = Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::True.as_u64());
    pub const FALSE: Value =
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | KeywordType::False.as_u64());

    const SIGN_BIT: u64 = 0x8000000000000000;

    pub fn from_obj_ref(obj: ObjRef) -> Value {
        Value(Self::SIGN_BIT | Self::NAN | obj.as_u64())
    }

    pub fn from_bool(b: bool) -> Value {
        Value::from_keyword_type(KeywordType::from_bool(b))
    }

    pub fn from_keyword_type(bt: KeywordType) -> Value {
        Value(Self::NAN | Self::TAG_KEYWORD_TYPE | bt.as_u64())
    }

    pub fn from_num(val: f64) -> Value {
        Value(unsafe { std::mem::transmute(val) })
    }

    pub fn as_obj_ref(self) -> ObjRef {
        #[cfg(feature = "safe_value")]
        assert!(self.is_obj_ref());

        let stripped = self.0 & !Self::SIGN_BIT & !Self::NAN & !Self::TAG_MASK;
        ObjRef::from_usize(stripped as usize)
    }

    pub fn as_str_ref(self) -> *mut StringRef {
        #[cfg(feature = "safe_value")]
        {
            assert!(self.is_obj_ref());
            assert!(self.as_obj_ref().is_str_ref())
        }
        self.as_obj_ref().as_str_ref()
    }

    pub fn as_str_ref_owned(self) -> StringRef {
        #[cfg(feature = "safe_value")]
        {
            assert!(self.is_obj_ref());
            assert!(self.as_obj_ref().is_str_ref())
        }
        unsafe { *self.as_obj_ref().as_str_ref() }
    }

    pub fn as_keyword_type(self) -> KeywordType {
        #[cfg(feature = "safe_value")]
        assert!(self.is_keyword_type());

        unsafe { std::mem::transmute((self.0 & 0b11111111) as u8) }
    }

    pub fn as_bool(self) -> bool {
        #[cfg(feature = "safe_value")]
        assert!(self.is_bool());
        self.is_true()
    }

    pub fn as_num(self) -> f64 {
        #[cfg(feature = "safe_value")]
        assert!(self.is_num());
        unsafe { std::mem::transmute(self.0) }
    }

    pub fn is_obj(self) -> bool {
        self.0 & (Self::SIGN_BIT | Self::NAN) == (Self::SIGN_BIT | Self::NAN)
    }

    pub fn is_str(self) -> bool {
        self.is_obj() && self.as_obj_ref().is_str_ref()
    }

    pub fn is_keyword_type(self) -> bool {
        self.0 & (Self::NAN | Self::TAG_KEYWORD_TYPE) == (Self::NAN | Self::TAG_KEYWORD_TYPE)
    }

    pub fn is_null(self) -> bool {
        self == Self::NULL
    }

    pub fn is_undefined(self) -> bool {
        self == Self::UNDEFINED
    }

    pub fn is_void(self) -> bool {
        self == Self::VOID
    }

    pub fn is_never(self) -> bool {
        self == Self::NEVER
    }

    pub fn is_bool(self) -> bool {
        self.0 & (Self::NAN | Self::TAG_KEYWORD_TYPE) == (Self::NAN | Self::TAG_KEYWORD_TYPE)
    }

    pub fn is_true(self) -> bool {
        self == Self::TRUE
    }

    pub fn is_false(self) -> bool {
        self == Self::FALSE
    }

    pub fn is_num(self) -> bool {
        self.0 & Self::NAN != Self::NAN
    }
}

pub struct ValueFormatterTs<'a> {
    pub depth: usize,
    pub pretty: bool,
    pub buf: &'a mut String,
    pub name: &'a str,
}

impl<'a> ValueFormatterTs<'a> {
    pub fn new_line(&mut self) -> fmt::Result {
        if self.pretty {
            self.buf.write_char('\n')?;
            for _ in 0..self.depth {
                self.buf.write_str("  ")?
            }
        }
        Ok(())
    }

    pub fn to_json(&mut self, val: Value) -> fmt::Result {
        if self.depth == 0 {
            write!(self.buf, "type {} = ", self.name)?;
        }

        if val.is_keyword_type() {
            return self.buf.write_str(match val.as_keyword_type() {
                KeywordType::Null => "null",
                KeywordType::Undefined => "undefined",
                KeywordType::Void => "void",
                KeywordType::Never => "never",
                KeywordType::String => "string",
                KeywordType::Number => "number",
                KeywordType::Bool => "bool",
                KeywordType::Object => "object",
                KeywordType::True => "true",
                KeywordType::False => "false",
            });
        }

        if val.is_num() {
            return write!(self.buf, "{}", val.as_num());
        }

        if val.is_str() {
            let str_ref = val.as_str_ref_owned();
            let str = str_ref.as_slice();
            return self.buf.write_str(str);
        }

        if val.is_obj() {
            let obj_ref = val.as_obj_ref();
            let obj = obj_ref.as_obj_ref();
            if obj.fields.is_empty() {
                return write!(self.buf, "{{}}");
            }
            write!(self.buf, "{{")?;
            self.depth += 1;
            let last = obj.fields.len().saturating_sub(1);
            for (i, field) in obj.fields.iter().enumerate() {
                self.new_line()?;
                let key = field.0;
                let value = field.1;
                write!(self.buf, "{}", unsafe {
                    (*key).as_str().escape_default().collect::<String>()
                })?;
                write!(self.buf, ": ")?;
                self.to_json(value)?;
                if i != last {
                    write!(self.buf, ",")?;
                }
            }
            self.depth -= 1;
            self.new_line()?;
            write!(self.buf, "}}")?;
            return Ok(());
        }

        unreachable!("Value: {:?}", val)
    }
}

#[cfg(test)]
mod test {
    use std::{
        alloc::{alloc, Layout},
        ptr::addr_of_mut,
    };

    use super::*;

    #[test]
    fn test_str_inlined() {
        let str = "HELLO";
        let mut str_ref = StringRef::new_inlined(str);
        assert!(!str_ref.is_heap_allocated());
        assert!(str_ref.is_inlined());
        assert_eq!(str, str_ref.as_inlined().as_slice());
        let str_ref_ptr = addr_of_mut!(str_ref);
        let obj = ObjRef::from_str_ref(str_ref_ptr);
        assert!(obj.is_str_ref());
        let str_ref_ptr_back = obj.as_str_ref();
        unsafe { assert_eq!((*str_ref_ptr_back).as_inlined().as_slice(), str) }
        let val = Value::from_obj_ref(obj);
        let str_ref_ptr_back = val.as_obj_ref().as_str_ref();
        unsafe { assert_eq!((*str_ref_ptr_back).as_inlined().as_slice(), str) }
    }

    #[test]
    fn test_str_heap() {
        let str = "HELLO";
        let mut str_layout = Layout::for_value(str.as_bytes()).align_to(16).unwrap();
        let chars = unsafe { alloc(str_layout) };
        unsafe {
            std::ptr::copy_nonoverlapping(str.as_bytes().as_ptr(), chars, str.as_bytes().len());
        }

        let mut str_ref = StringRef::new_heap(chars, str.as_bytes().len());
        assert!(str_ref.is_heap_allocated());
        assert!(!str_ref.is_inlined());
        assert_eq!(str, str_ref.as_heap().as_slice());
        let str_ref_ptr = addr_of_mut!(str_ref);
        let obj = ObjRef::from_str_ref(str_ref_ptr);
        assert!(obj.is_str_ref());
        let str_ref_ptr_back = obj.as_str_ref();
        unsafe { assert_eq!((*str_ref_ptr_back).as_heap().as_slice(), str) }
        let val = Value::from_obj_ref(obj);
        let str_ref_ptr_back = val.as_obj_ref().as_str_ref();
        unsafe { assert_eq!((*str_ref_ptr_back).as_heap().as_slice(), str) }
    }

    #[test]
    fn test_value_obj() {
        let obj = Object { fields: vec![] }.alloc();
        let obj_ref = ObjRef::from_obj(obj);
        let val = Value::from_obj_ref(obj_ref);
        assert!(val.is_obj());
        assert!(val.as_obj_ref().is_obj());
        assert!(!val.as_obj_ref().is_str_ref());
    }

    #[test]
    fn test_value_obj_str() {
        let str = "foo";
        let str_layout = Layout::for_value(str.as_bytes()).align_to(16).unwrap();
        let chars = unsafe { alloc(str_layout) };
        unsafe {
            std::ptr::copy_nonoverlapping(str.as_bytes().as_ptr(), chars, str.as_bytes().len());
        }
        let mut str_ref = StringRef::new_heap(chars, str.as_bytes().len());
        let field1_key = addr_of_mut!(str_ref);
        let field1_val = Value::from_num(420.69f64);
        let obj_fields: Vec<ObjectField> = vec![(field1_key, field1_val).into()];
        let mut obj = Object { fields: obj_fields };
        let obj_ptr = obj.alloc();
        let obj_ref = ObjRef::from_obj(obj_ptr);

        let value = Value::from_obj_ref(obj_ref);
        assert!(value.is_obj());
        assert!(!value.is_bool());
        assert!(!value.is_keyword_type());
        assert!(!value.is_num());
        let obj_ref_back = value.as_obj_ref();
        assert_eq!(obj_ref_back.as_obj() as usize, obj_ptr as usize);
        // let derefed_val = unsafe { *(obj_ref_back.0 as *mut usize) };
        // assert_eq!(derefed_val, val)
    }

    #[test]
    fn test_value_num() {
        let num = 420.69f64;
        let val = Value::from_num(num);
        assert!(val.is_num());
        assert!(!val.is_obj());
        assert!(!val.is_bool());
        assert!(!val.is_keyword_type());

        assert_eq!(val.as_num(), num);
    }

    #[test]
    fn test_value_bool() {
        let b = true;
        let val = Value::from_keyword_type(KeywordType::from_bool(b));

        assert!(val.is_bool());
        assert!(!val.is_num());
        assert!(!val.is_obj());
        assert!(val.is_keyword_type());

        assert_eq!(val.as_bool(), b);
    }
}
