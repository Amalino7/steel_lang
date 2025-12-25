use crate::vm::bytecode::Chunk;
use crate::vm::gc::Gc;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
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

#[derive(Clone, Debug, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    String(Gc<String>),
    Closure(Gc<Closure>),
    Function(Gc<Function>),
    NativeFunction(NativeFn),
    BoundMethod(Gc<BoundMethod>),
    Instance(Gc<Instance>),
    Enum(Gc<EnumVariant>),
    InterfaceObj(Gc<InterfaceObj>),
}

pub type NativeFn = fn(&[Value]) -> Value;

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
            (Value::String(l), Value::String(r)) => l.as_str() == r.as_str(),
            (Value::Boolean(l), Value::Boolean(r)) => l == r,
            (Value::Nil, Value::Nil) => true,
            (Value::Enum(l), Value::Enum(r)) => l.enum_name == r.enum_name && l.tag == r.tag,
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
            Value::Nil => write!(f, "Nil"),
            Value::Function(func) => write!(f, "<fn {}>", func.name.as_str()),
            Value::NativeFunction(_) => write!(f, "<native fn>"),
            Value::Closure(closure) => write!(f, "<closure {}>", closure.function.name.as_str()),
            Value::Instance(instance) => write!(f, "<instance {}>", instance.name),
            Value::BoundMethod(bound_method) => write!(
                f,
                "<bound method {} of {}>",
                bound_method.method.name.as_str(),
                bound_method.receiver
            ),
            Value::Enum(enum_variant) => write!(f, "<enum {}>", enum_variant.enum_name),
            Value::InterfaceObj(_) => write!(f, "<interface>"),
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
