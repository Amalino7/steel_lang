use crate::vm::bytecode::Chunk;
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};
use std::rc::Rc;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub arity: usize,
    pub chunk: Chunk,
}

impl Function {
    pub fn new(name: String, arity: usize, chunk: Chunk) -> Function {
        Function { name, arity, chunk }
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    String(Rc<String>),
    Function(Rc<Function>),
    Boolean(bool),
    Nil,
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(l), Value::Number(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(str) => write!(f, "{}", str),
            Value::Boolean(bool) => write!(f, "{}", bool),
            Value::Nil => write!(f, "Nil"),
            Value::Function(func) => write!(f, "<fn {}>", func.name.as_str()),
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
            (Value::String(l), Value::String(r)) => Value::String(Rc::new(format!("{}{}", l, r))),
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
