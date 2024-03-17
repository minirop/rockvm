use std::any::Any;
use std::fmt;
use std::cmp::Ordering;
use std::ops::*;
use crate::Fiber;
use crate::Opcode;
use crate::Function;
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::HashMap;

use crate::NativeFunction;

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Rc<str>,
    pub supertype: Option<Rc<str>>,
    pub fields: HashMap<Rc<str>, Vec<Opcode>>,
    pub functions: HashMap<Rc<str>, Function>,
    pub natives: HashMap<Rc<str>, NativeFunction>,
}

#[derive(Debug, Clone)]
pub struct Object {
    pub class: Rc<str>,
    pub fields: HashMap<Rc<str>, Value>,
}

#[derive(Debug)]
pub struct Pointer {
    pub class: Rc<str>,
    pub data: Box<dyn Any>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Integer(i32),
    Float(f32),
    String(Rc<str>),
    Class(Rc<RefCell<Class>>),
    Object(Rc<RefCell<Object>>),
    List(Rc<RefCell<Vec<Value>>>),
    Closure(Function),
    Fiber(Rc<RefCell<Fiber>>),
    Pointer(Rc<RefCell<Pointer>>),
}

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            Value::Integer(i) => {
                match other {
                    Value::Integer(i2) => Value::Integer(i + i2),
                    Value::Float(f) => Value::Float(i as f32 + f),
                    Value::String(string) => Value::String(format!("{i}{string}").into()),
                    _ => panic!("{} + {}", self, other),
                }
            },
            Value::Float(f) => {
                match other {
                    Value::Integer(i) => Value::Float(f + i as f32),
                    Value::Float(f2) => Value::Float(f + f2),
                    Value::String(string) => Value::String(format!("{f}{string}").into()),
                    _ => panic!("{} + {}", self, other),
                }
            },
            Value::String(string) => {
                Value::String(format!("{string}{other}").into())
            },
            _ => match other {
                Value::String(string) => Value::String(format!("{self}{string}").into()),
                _ => panic!("{} + {}", self, other)
            },
        }
    }
}

impl Sub for Value {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match self {
            Value::Integer(i) => {
                match other {
                    Value::Integer(i2) => Value::Integer(i - i2),
                    Value::Float(f) => Value::Float(i as f32 - f),
                    _ => panic!("{} - {}", self, other),
                }
            },
            Value::Float(f) => {
                match other {
                    Value::Integer(i) => Value::Float(f - i as f32),
                    Value::Float(f2) => Value::Float(f - f2),
                    _ => panic!("{} - {}", self, other),
                }
            },
            _ => panic!("{} - {}", self, other),
        }
    }
}

impl Mul for Value {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match self {
            Value::Integer(i) => {
                match other {
                    Value::Integer(i2) => Value::Integer(i * i2),
                    Value::Float(f) => Value::Float(i as f32 * f),
                    _ => panic!("{} - {}", self, other),
                }
            },
            Value::Float(f) => {
                match other {
                    Value::Integer(i) => Value::Float(f * i as f32),
                    Value::Float(f2) => Value::Float(f * f2),
                    _ => panic!("{} * {}", self, other),
                }
            },
            Value::String(string) => {
                match other {
                    Value::Integer(i) => Value::String(format!("{}", string.repeat(i as usize)).into()),
                    _ => panic!("'{}' + '{}'", string, other),
                }
            },
            _ => panic!("{} * {}", self, other),
        }
    }
}

impl Div for Value {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match self {
            Value::Integer(i) => {
                match other {
                    Value::Integer(i2) => Value::Integer(i / i2),
                    Value::Float(f) => Value::Float(i as f32 / f),
                    _ => panic!("{} / {}", self, other),
                }
            },
            Value::Float(f) => {
                match other {
                    Value::Integer(i) => Value::Float(f / i as f32),
                    Value::Float(f2) => Value::Float(f / f2),
                    _ => panic!("{} / {}", self, other),
                }
            },
            _ => panic!("{} / {}", self, other),
        }
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Value::Bool(b) => Value::Bool(!b),
            Value::Null => Value::Bool(true),
            _ => Value::Bool(false),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => l == r,
            (Value::Float(l), Value::Float(r)) => l == r,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            (Value::Null, Value::Null) => true,
            _ => panic!("{} == {}", self, other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return None;
        }

        match (self, other) {
            (Value::Integer(l), Value::Integer(r)) => l.partial_cmp(r),
            (Value::Float(l), Value::Float(r)) => l.partial_cmp(r),
            (Value::Bool(l), Value::Bool(r)) => l.partial_cmp(r),
            (Value::String(l), Value::String(r)) => l.partial_cmp(r),
            (Value::Null, Value::Null) => Some(Ordering::Equal),
            _ => panic!("{:?} cmp {:?}", self, other),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Float(float) => write!(f, "{}", float),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::String(s) => write!(f, "{}", s),
            Value::Integer(i) => write!(f, "{}", i),
            Value::Class(c) => write!(f, "<class '{}'>", c.borrow().name),
            Value::Object(o) => write!(f, "<object of type '{}'>", o.borrow().class),
            Value::List(list) => {
                write!(f, "[").unwrap();
                for i in 0..list.borrow().len() {
                    if i > 0 { write!(f, ", ").unwrap(); }
                    write!(f, "{}", list.borrow()[i]).unwrap();
                }
                write!(f, "]")
            },
            Value::Closure(_) => write!(f, "<closure>"),
            Value::Fiber(_) => write!(f, "<fiber>"),
            Value::Pointer(p) => write!(f, "<Pointer of type '{}'>", p.borrow().class),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Float(f) => Value::Float(-f),
            Value::Integer(i) => Value::Integer(-i),
            _ => panic!("Can't negate {}", self),
        }
    }
}
