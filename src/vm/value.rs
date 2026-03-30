use crate::vm::bytecode::Chunk;
use crate::vm::gc::{GarbageCollector, Gc};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub chunk: Chunk,
}

#[derive(Debug)]
pub struct Closure {
    pub function: Gc<Function>,
    pub captures: Vec<Value>,
}

pub struct Instance {
    pub name: Value, // Name of the struct useful for debugging
    pub fields: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub enum_name: Value, // Name for debugging purposes
    pub tag: usize,
    pub payload: Value,
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: Gc<Function>,
}

#[derive(Debug)]
pub struct VTable {
    pub methods: Vec<Gc<Function>>,
}

#[derive(Debug)]
pub struct InterfaceObj {
    pub data: Value,
    pub vtable: Gc<VTable>,
}

impl Function {
    pub fn new(name: String, chunk: Chunk) -> Function {
        Function { name, chunk }
    }
}
#[derive(Debug)]
pub struct List {
    pub vec: Vec<Value>,
}

#[derive(Debug)]
pub struct Map {
    pub map: HashMap<HashableValue, Value>,
}

/// A wrapper that makes Value usable as a HashMap key.
/// Hashing rules:
/// - Number: hash via `f64::to_bits()` (NaN is disallowed)
/// - String/Boolean/Nil: hash by content
/// - Heap values (List, Map): identity hash (pointer address)
#[derive(Debug, Clone)]
pub struct HashableValue(pub Value);

/// This needs to be different from normal Value Eq since hashmaps rely on stable values.
impl PartialEq for HashableValue {
    fn eq(&self, other: &Self) -> bool {
        match (self.0, other.0) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r || l.as_str() == r.as_str(),
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::List(l), Value::List(r)) => l == r,
            (Value::Map(l), Value::Map(r)) => l == r,
            (Value::Instance(l), Value::Instance(r)) => l == r,
            (Value::Enum(l), Value::Enum(r)) => l == r,
            (Value::Closure(l), Value::Closure(r)) => l == r,
            (Value::Function(l), Value::Function(r)) => l == r,
            (Value::NativeFunction(l), Value::NativeFunction(r)) => std::ptr::fn_addr_eq(l, r),
            (Value::BoundMethod(l), Value::BoundMethod(r)) => l == r,
            (Value::InterfaceObj(l), Value::InterfaceObj(r)) => l == r,
            _ => false,
        }
    }
}

impl Eq for HashableValue {}

impl Hash for HashableValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match &self.0 {
            Value::Number(f) => {
                0u8.hash(state);
                // Normalize 0 since, 0 == -0 but they have different bit representations
                if *f == 0.0 || *f == -0.0 {
                    0.hash(state);
                } else {
                    f.to_bits().hash(state);
                }
            }
            Value::String(s) => {
                1u8.hash(state);
                s.as_str().hash(state);
            }
            Value::Boolean(b) => {
                2u8.hash(state);
                b.hash(state);
            }
            Value::Nil => {
                3u8.hash(state);
            }
            Value::List(gc) => {
                4u8.hash(state);
                gc.address().hash(state);
            }
            Value::Map(gc) => {
                5u8.hash(state);
                gc.address().hash(state);
            }
            Value::Instance(gc) => {
                6u8.hash(state);
                gc.address().hash(state);
            }
            Value::Enum(gc) => {
                7u8.hash(state);
                gc.address().hash(state);
            }
            Value::Closure(gc) => {
                8u8.hash(state);
                gc.address().hash(state);
            }
            Value::Function(gc) => {
                9u8.hash(state);
                gc.address().hash(state);
            }
            Value::NativeFunction(f) => {
                10u8.hash(state);
                f.hash(state);
            }
            Value::BoundMethod(gc) => {
                11u8.hash(state);
                gc.address().hash(state);
            }
            Value::InterfaceObj(gc) => {
                12u8.hash(state);
                gc.address().hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    List(Gc<List>),
    Map(Gc<Map>),
    String(Gc<String>),
    Closure(Gc<Closure>),
    Function(Gc<Function>),
    NativeFunction(NativeFn),
    BoundMethod(Gc<BoundMethod>),
    Instance(Gc<Instance>),
    Enum(Gc<EnumVariant>),
    InterfaceObj(Gc<InterfaceObj>),
}

pub type NativeFn = fn(&[Value], &mut GarbageCollector) -> Result<Value, String>;

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r || l.as_str() == r.as_str(),
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Enum(l), Value::Enum(r)) => {
                l.enum_name == r.enum_name && l.tag == r.tag && l.payload == r.payload
            }
            (Value::InterfaceObj(l), Value::InterfaceObj(r)) => l == r || l.data == r.data,
            (Value::Instance(l), Value::Instance(r)) => {
                if l == r {
                    return true;
                }
                l.name == r.name
                    && l.fields.len() == r.fields.len()
                    && l.fields.iter().zip(r.fields.iter()).all(|(a, b)| a == b)
            }
            (Value::List(l), Value::List(r)) => {
                if l == r {
                    return true;
                }
                l.vec.len() == r.vec.len() && l.vec.iter().zip(r.vec.iter()).all(|(a, b)| a == b)
            }
            // Map equality: identity only (pointer equality handled by Gc<T>'s PartialEq)
            (Value::Map(l), Value::Map(r)) => l == r,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str.as_str()),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Nil => write!(f, "nil"),
            Value::Function(func) => write!(f, "<fn {}>", func.name.as_str()),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Closure(closure) => write!(f, "<closure {} @>", closure.function.name.as_str()),
            Value::Instance(instance) => write!(f, "<instance {} @{}>", instance.name, instance),
            Value::BoundMethod(bound_method) => write!(
                f,
                "<bound method {} of {}>",
                bound_method.method.name.as_str(),
                bound_method.receiver
            ),
            Value::Enum(enum_variant) => write!(f, "<enum {}>", enum_variant.enum_name),
            Value::InterfaceObj(_) => write!(f, "<interface>"),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, val) in list.vec.iter().enumerate() {
                    write!(f, "{}", val)?;
                    if i != list.vec.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "[")?;
                let mut iter = map.map.iter().peekable();
                while let Some((k, v)) = iter.next() {
                    write!(f, "{}: {}", k.0, v)?;
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

impl Sub for Value {
    type Output = Value;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l - r),
            (l, r) => {
                unreachable!(
                    "Invalid type for subtraction!\
                     Left: {:?}, Right: {:?}",
                    l, r
                );
            }
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l * r),
            (l, r) => {
                unreachable!(
                    "Invalid type for multiplication!\
                     Left: {:?}, Right: {:?}",
                    l, r
                );
            }
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l / r),
            (l, r) => {
                unreachable!(
                    "Invalid type for division!\
                     Left: {:?}, Right: {:?}",
                    l, r
                );
            }
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Value::Number(-n),
            n => {
                unreachable!(
                    "Invalid type for negation, expected number.!\
                     Val: {:?}",
                    n
                );
            }
        }
    }
}

impl Not for Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Boolean(a) => Value::Boolean(!a),
            n => {
                unreachable!(
                    "Invalid type for negation, expected number.!\
                     Val: {:?}",
                    n
                );
            }
        }
    }
}

impl Add for Value {
    type Output = Value;
    #[inline]
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Value::Number(l + r),
            (l, r) => {
                unreachable!(
                    "Invalid type for addition!\
                     Left: {:?}, Right: {:?}",
                    l, r
                );
            }
        }
    }
}
