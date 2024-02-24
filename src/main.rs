use std::cell::RefCell;
use std::rc::Rc;
use std::io::Read;
use std::fs::File;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::ops::*;
use std::ops::Neg;
use std::fmt;
use byteorder::{ReadBytesExt, LittleEndian};
use clap::Parser;

#[derive(Debug, Clone)]
enum Opcode {
    Return,
    Constant(Value),
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    True,
    False,
    Null,
    Not,
    Equal,
    LowerThan,
    GreaterThan,
    Print,
    Pop,
    LoadModuleVar(String),
    StoreModuleVar(String),
    Call(String, u8),
    CallStatic(String, u8),
    LoadLocalVar(usize),
    StoreLocalVar(usize),
    AllocateVar(String),
    LoadFieldThis(String),
    StoreFieldThis(String),
    JumpIf(i8),
    Jump(i8),
    Dup,

    DumpStack,
}

const OP_RETURN: u8 = 1;
const OP_CONSTANT: u8 = 2;
const OP_NEGATE: u8 = 3;
const OP_ADD: u8 = 4;
const OP_SUB: u8 = 5;
const OP_MUL: u8 = 6;
const OP_DIV: u8 = 7;
const OP_TRUE: u8 = 8;
const OP_FALSE: u8 = 9;
const OP_NULL: u8 = 10;
const OP_NOT: u8 = 11;
const OP_EQUAL: u8 = 12;
const OP_LOWER_THAN: u8 = 13;
const OP_GREATER_THAN: u8 = 14;
const OP_PRINT: u8 = 15;
const OP_POP: u8 = 16;
const OP_LOAD_MODULE_VAR: u8 = 17;
const OP_STORE_MODULE_VAR: u8 = 18;
const OP_CALL: u8 = 19;
const OP_CALL_STATIC: u8 = 20;
const OP_LOAD_LOCAL_VAR: u8 = 21;
const OP_STORE_LOCAL_VAR: u8 = 22;
const OP_ALLOCATE_VAR: u8 = 23;
const OP_LOAD_FIELD_THIS: u8 = 24;
const OP_STORE_FIELD_THIS: u8 = 25;
const OP_JUMP_IF: u8 = 26;
const OP_JUMP: u8 = 27;
const OP_DUP: u8 = 28;
const OP_DUMP_STACK: u8 = 29;


#[derive(Debug, Clone)]
struct Class {
    name: String,
    supertype: Option<String>,
    fields: HashMap<String, Vec<Opcode>>,
    functions: HashMap<String, Function>,
    natives: HashMap<String, NativeFunction>,
}

#[derive(Debug, Clone)]
struct Object {
    class: String,
    fields: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
enum Value {
    Null,
    Bool(bool),
    Integer(i32),
    Float(f32),
    String(String),
    Class(Rc<RefCell<Class>>),
    Object(Rc<RefCell<Object>>),
    List(Vec<Value>),
}

const VAL_NULL: u8 = 1;
const VAL_BOOL: u8 = 2;
const VAL_INTEGER: u8 = 3;
const VAL_FLOAT: u8 = 4;
const VAL_STRING: u8 = 5;
const VAL_CLASS: u8 = 6;

impl Add for Value {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match self {
            Value::Integer(i) => {
                match other {
                    Value::Integer(i2) => Value::Integer(i + i2),
                    Value::Float(f) => Value::Float(i as f32 + f),
                    _ => panic!("{} + {}", self, other),
                }
            },
            Value::Float(f) => {
                match other {
                    Value::Integer(i) => Value::Float(f + i as f32),
                    Value::Float(f2) => Value::Float(f + f2),
                    _ => panic!("{} + {}", self, other),
                }
            },
            Value::String(string) => {
                match other {
                    Value::String(string2) => Value::String(format!("{}{}", string, string2)),
                    Value::Integer(i) => Value::String(format!("{}{}", string, i)),
                    Value::Float(f) => Value::String(format!("{}{}", string, f)),
                    _ => panic!("'{}' + '{}'", string, other),
                }
            },
            _ => panic!("{} + {}", self, other),
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
                    Value::Integer(i) => Value::String(format!("{}", string.repeat(i as usize))),
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
            Value::Float(f) => Value::Bool(f == 0.0),
            Value::Bool(b) => Value::Bool(!b),
            Value::Null => Value::Bool(true),
            _ => panic!("NOT {}", self),
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
            Value::Object(o) => write!(f, "<object of '{}'>", o.borrow().class),
            Value::List(list) => write!(f, "<list '{}'>", list.len()),
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Float(f) => Value::Float(-f),
            _ => panic!("Can't negate {}", self),
        }
    }
}


#[derive(Debug, Clone)]
#[allow(unused)]
struct Function {
    name: String,
    arity: u8,
    //is_static: bool,
    locals: u8,
    code: Chunk,
}

impl Function {
    #[allow(unused)]
    fn disassemble(&self) {
        println!("===== {} =====", self.name);
        for op in &self.code.code {
            println!("{:?}", op);
        }
        println!("==== /{} =====", self.name);
    }
}

#[derive(Debug, Clone)]
#[allow(unused)]
struct NativeFunction {
    name: String,
    arity: u8,
    //is_static: bool,
    code: fn(&mut Vm),
}

enum InterpretResult {
    Ok,
}

#[derive(Debug, Clone)]
struct Chunk {
    code: Vec<Opcode>,
    ip: usize,
}

impl Chunk {
    fn new() -> Self {
        Self {
            code: vec![],
            ip: 0,
        }
    }
}

struct Vm {
    callstack: Vec<Function>,
    stackbase: Vec<usize>,
    stack: Vec<Value>,
    variables: HashMap<String, Value>,
}

impl Vm {
    fn new() -> Self {
        Self {
            callstack: vec![],
            stackbase: vec![],
            stack: vec![],
            variables: HashMap::new(),
        }
    }

    fn push_callframe(&mut self, function: Function) {
        //function.disassemble();

        for _ in 0..function.locals {
            self.push_null();
        }

        self.stackbase.push(self.stack.len() - 1 - function.arity as usize);
        self.callstack.push(function);
    }

    fn pop_callframe(&mut self) {
        self.callstack.pop().unwrap();
        self.stackbase.pop().unwrap();
    }

    fn load(&mut self, filename: &str) -> std::io::Result<()> {
        let mut f = File::open(filename)?;
        let magic = f.read_u32::<LittleEndian>()?;
        if magic != 0x4B434F52 {
            panic!("Wrong magic bytes.");
        }

        let version = f.read_u8()?; assert_eq!(version, 1);

        let mut strings = vec![];
        let strings_len = f.read_u32::<LittleEndian>()?;
        for _ in 0..strings_len {
            strings.push(read_string(&mut f));
        }

        let classes_len = f.read_u32::<LittleEndian>()?;
        for _ in 0..classes_len {
            let name = read_string(&mut f);
            let supertype = read_string(&mut f);
            let fields_len = f.read_u8()?;

            let mut fields = HashMap::new();
            for _ in 0..fields_len {
                let name = read_string(&mut f);
                let size = f.read_u8()? as usize;

                let mut default: Vec<Opcode> = vec![];
                let mut buf = vec![0u8; size];
                f.read(&mut buf).unwrap();

                let mut i = 0;
                while i < buf.len() {
                    let c = buf[i];
                    i += 1;

                    default.push(self.decode(&name, c, &mut buf, &mut i, &strings)?);
                }

                fields.insert(name.clone(), default);
            }

            let functions_len = f.read_u8()?;

            let mut functions = HashMap::new();
            for _ in 0..functions_len {
                let name = read_string(&mut f);
                let arity = f.read_u8()?;
                //let is_static = f.read_u8()? > 0;
                let locals = f.read_u8()?;
                
                let size = f.read_u16::<LittleEndian>()? as usize;
                let mut buf = vec![0u8; size];
                f.read(&mut buf).unwrap();
                let mut code = Chunk::new();

                let mut i = 0;
                while i < buf.len() {
                    let c = buf[i];
                    i += 1;

                    code.code.push(self.decode(&name, c, &mut buf, &mut i, &strings)?);
                }

                functions.insert(name.clone(), Function {
                    name, arity, locals, code, //is_static
                });
            }

            let supertype = if supertype.len() > 0 { Some(supertype) } else { Some("Object".into()) };
            self.variables.insert(name.clone(), Value::Class(Rc::new(RefCell::new(Class {
                name: name.clone(), supertype,
                fields, functions, natives: HashMap::new(),
            }))));
        }

        Ok(())
    }

    fn decode(&self, name: &str, c: u8, buf: &mut Vec<u8>, i: &mut usize, strings: &Vec<String>) -> std::io::Result<Opcode> {
        let ret = match c {
            OP_RETURN => Opcode::Return,
            OP_CONSTANT => {
                let t = buf[*i];
                let val = match t {
                    VAL_NULL => Value::Null,
                    VAL_BOOL => {
                        *i += 1;
                        let v = buf[*i];
                        Value::Bool(v > 0)
                    },
                    VAL_INTEGER => {
                        *i += 5;
                        let int = (&buf[(*i - 4)..*i]).read_i32::<LittleEndian>()?;
                        Value::Integer(int)
                    },
                    VAL_FLOAT => {
                        *i += 5;
                        let flt = (&buf[(*i - 4)..*i]).read_f32::<LittleEndian>()?;
                        Value::Float(flt)
                    },
                    VAL_STRING => {
                        *i += 3;
                        let int = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                        Value::String(strings[int].clone())
                    },
                    VAL_CLASS => panic!("class not handled."),
                    _ => panic!("Unknown value type {t}."),
                };
                Opcode::Constant(val)
            },
            OP_NEGATE => Opcode::Negate,
            OP_ADD => Opcode::Add,
            OP_SUB => Opcode::Sub,
            OP_MUL => Opcode::Mul,
            OP_DIV => Opcode::Div,
            OP_TRUE => Opcode::True,
            OP_FALSE => Opcode::False,
            OP_NULL => Opcode::Null,
            OP_NOT => Opcode::Not,
            OP_EQUAL => Opcode::Equal,
            OP_LOWER_THAN => Opcode::LowerThan,
            OP_GREATER_THAN => Opcode::GreaterThan,
            OP_PRINT => Opcode::Print,
            OP_POP => Opcode::Pop,
            OP_LOAD_MODULE_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::LoadModuleVar(strings[idx].clone())
            },
            OP_STORE_MODULE_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::StoreModuleVar(strings[idx].clone())
            },
            OP_CALL => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                *i += 1;
                let args = buf[*i - 1];
                Opcode::Call(strings[idx].clone(), args)
            },
            OP_CALL_STATIC => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                *i += 1;
                let args = buf[*i - 1];
                Opcode::CallStatic(strings[idx].clone(), args)
            },
            OP_LOAD_LOCAL_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::LoadLocalVar(idx)
            },
            OP_STORE_LOCAL_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::StoreLocalVar(idx)
            },
            OP_DUMP_STACK => Opcode::DumpStack,
            OP_ALLOCATE_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::AllocateVar(strings[idx].clone())
            },
            OP_LOAD_FIELD_THIS => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::LoadFieldThis(strings[idx].clone())
            },
            OP_STORE_FIELD_THIS => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::StoreFieldThis(strings[idx].clone())
            },
            OP_JUMP_IF => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_i8()?;
                Opcode::JumpIf(offset)
            },
            OP_JUMP => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_i8()?;
                Opcode::Jump(offset)
            },
            OP_DUP => Opcode::Dup,
            _ => {
                println!("{:?}", buf);
                panic!("{name}: Unknown byte {:#X} at index {}", c, *i - 1);
            },
        };

        Ok(ret)
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            let func = self.callstack.last_mut().unwrap();
            func.code.ip += 1;
            let inst = func.code.code[func.code.ip - 1].clone();
            let stackbase = self.stackbase.last().unwrap().clone();

            match inst {
                Opcode::Return => {
                    let ret = self.pop();
                    while self.stack.len() > stackbase {
                        self.pop();
                    }
                    self.push(ret);

                    self.pop_callframe();
                    if self.callstack.len() == 0 {
                        assert_eq!(self.stack.len(), 1);
                        return InterpretResult::Ok;
                    }
                },
                Opcode::Constant(value) => self.push(value.clone()),
                Opcode::Negate => {
                    let val = self.pop();
                    self.push(-val);
                },
                Opcode::Add => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l + r);
                },
                Opcode::Sub => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l - r);
                },
                Opcode::Mul => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l * r);
                },
                Opcode::Div => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(l / r);
                },
                Opcode::True => self.push(Value::Bool(true)),
                Opcode::False => self.push(Value::Bool(false)),
                Opcode::Null => self.push_null(),
                Opcode::Not => {
                    let val = self.pop();
                    self.push(!val);
                },
                Opcode::Equal => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(Value::Bool(l == r));
                },
                Opcode::LowerThan => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(Value::Bool(l < r));
                },
                Opcode::GreaterThan => {
                    let r = self.pop();
                    let l = self.pop();
                    self.push(Value::Bool(l > r));
                },
                Opcode::Print => println!("{}", self.pop()),
                Opcode::Pop => { self.pop(); },
                Opcode::LoadModuleVar(name) => {
                    let var = match self.variables.get(&name) {
                        Some(val) => val,
                        None => panic!("Unknown variable '{}'.", name),
                    };
                    self.push(var.clone());
                },
                Opcode::Call(name, args) => {
                    let name = name.clone();
                    let index = self.stack.len() - (args as usize) - 1;
                    let top = self.stack[index].clone();

                    let class = match top {
                        Value::Class(c) => c,
                        Value::Object(o) => {
                            let Some(Value::Class(c)) = self.variables.get(&o.borrow().class) else {
                                panic!("Object of unknown class: {}", o.borrow().class);
                            };

                            c.clone()
                        },
                        Value::String(_) => {
                            let Some(Value::Class(c)) = self.variables.get("String") else {
                                panic!("");
                            };

                            c.clone()
                        },
                        Value::List(_) => {
                            let Some(Value::Class(c)) = self.variables.get("List") else {
                                panic!("");
                            };

                            c.clone()
                        },
                        _ => panic!("Calling '{name}' with {:?}.", top),
                    };

                    self.call_method(&class.borrow(), &name, args);
                },
                Opcode::LoadLocalVar(index) => {
                    let idx = stackbase + index as usize;
                    let var = self.stack[idx].clone();
                    self.push(var);
                },
                Opcode::StoreLocalVar(index) => {
                    let value = self.pop();
                    let idx = stackbase + index as usize;
                    self.stack[idx] = value;
                    self.push_null();
                },
                Opcode::AllocateVar(name) => {
                    let mut fields = HashMap::new();
                    let mut parent_name = name.clone();
                    while parent_name.len() > 0 {
                        let class = match self.variables.get(&parent_name) {
                            Some(val) => {
                                if let Value::Class(class) = val {
                                    class
                                } else {
                                    panic!("'{name}' is not a class.");
                                }
                            },
                            None => panic!("Unknown variable '{}'.", name),
                        };

                        for (name, default) in &class.borrow().fields {
                            let value = self.initialise_field(default);
                            fields.insert(name.clone(), value);
                        }

                        if let Some(parent) = &class.borrow().supertype {
                            parent_name = parent.clone();
                        } else {
                            parent_name = "".into();
                        }
                    }

                    self.push(Value::Object(Rc::new(RefCell::new(Object {
                        class: name,
                        fields,
                    }))));
                },
                Opcode::StoreModuleVar(name) => {
                    let var = self.pop();
                    self.variables.insert(name, var);
                    self.push_null();
                },
                Opcode::LoadFieldThis(name) => {
                    let val = &self.stack[stackbase];
                    let Value::Object(obj) = val else {
                        panic!("{:?} => {name}", val);
                    };

                    let var = obj.borrow().fields.get(&name).unwrap().clone();
                    self.push(var);
                },
                Opcode::StoreFieldThis(name) => {
                    let var = self.pop();
                    let val = self.stack.get_mut(stackbase).unwrap();
                    let Value::Object(ref mut obj) = val else {
                        panic!("{:?} => {name}", val);
                    };

                    if let Some(field) = obj.borrow_mut().fields.get_mut(&name) {
                        *field = var.clone();
                    }

                    self.push(var);
                },
                Opcode::Jump(offset) => {
                    let newip = (func.code.ip as i32 + offset as i32) as usize;
                    func.code.ip = newip;
                },
                Opcode::JumpIf(offset) => {
                    let var = self.stack.pop().unwrap();
                    let cond = match var {
                        Value::Bool(b) => b,
                        Value::Integer(i) => i != 0,
                        _ => panic!("false"),                        
                    };

                    if cond {
                        let newip = (func.code.ip as i32 + offset as i32) as usize;
                        func.code.ip = newip;
                    }
                },
                Opcode::Dup => {
                    let var = self.stack.last().unwrap();
                    self.push(var.clone());
                },
                Opcode::DumpStack => self.dump_stack("opcode"),
                _ => panic!("Unhandled: {:?}", inst),
            };
        }
    }

    fn initialise_field(&self, code: &Vec<Opcode>) -> Value {
        let mut id = 0;
        let mut stack = vec![];
        while id < code.len() {
            let op = &code[id];
            match op {
                Opcode::Constant(c) => {
                    stack.push(c);
                },
                _ => panic!("{:?} is not allowed in field initialiser", op),
            };
            id += 1;
        }

        assert_eq!(stack.len(), 1);

        stack[0].clone()
    }

    fn call_method(&mut self, class: &Class, name: &str, args: u8) {
        if let Some(func) = class.functions.get(name) {
            self.call(func, args);
            return;
        }

        if let Some(func) = class.natives.get(name) {
            self.call_native(func, args);
            return;
        }

        if let Some(supertype_name) = &class.supertype {
            let Some(supertype) = self.variables.get(supertype_name) else {
                panic!("'{}' not found.", supertype_name);
            };
            let Value::Class(supertype_class) = supertype else {
                panic!("'{}' is not a class.", supertype_name);
            };

            let supertype_class = supertype_class.clone();
            self.call_method(&supertype_class.borrow(), name, args);
            return;
        }

        panic!("'{}' doesn't have function named '{}'.", class.name, name);
    }

    fn call(&mut self, func: &Function, args: u8) {
        if args != func.arity {
            panic!("{} call: {} vs {}", func.name, func.arity, args);
        }

        self.push_callframe(func.clone());
    }

    fn call_native(&mut self, func: &NativeFunction, args: u8) {
        if args != func.arity {
            panic!("{} call: {} vs {}", func.name, func.arity, args);
        }

        (func.code)(self);

        let ret = self.pop();
        for _ in 0..(func.arity + 1) {
            self.pop();
        }
        self.push(ret);
    }

    fn push(&mut self, value: Value) {
        //println!("PUSH: {value}");
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        let top = self.stack.pop().unwrap();
        //println!("POP: {}", top);
        top
    }

    fn push_null(&mut self) {
        self.push(Value::Null);
    }

    fn dump_stack(&self, name: &str) {
        println!("===== STACK {name} =====");
        for s in &self.stack {
            println!("- {}", s);
        }
        println!("=====  END  {name} =====");
    }

    fn disassemble_all(&self) {
        for (_, v) in &self.variables {
            match v {
                Value::Class(c) => {
                    print!("class {}", c.borrow().name);
                    if let Some(supertype) = &c.borrow().supertype {
                        print!(" extends {}", supertype);
                    }
                    println!(":");
                    for (name, default) in &c.borrow().fields {
                        println!("\tfield {name}: {:?}", default);
                    }
                    for (name, fun) in &c.borrow().functions {
                        println!("\tfunction {name}:");
                        for op in &fun.code.code {
                            println!("\t\t{:?}", op);
                        }
                    }
                    for (name, fun) in &c.borrow().natives {
                        println!("\tfunction {name}: {:?}", fun.code);
                    }

                    if c.borrow().fields.len() == 0 && c.borrow().functions.len() == 0 && c.borrow().natives.len() == 0 {
                        println!("\tpass");
                    }
                    println!("");
                },
                _ => panic!("{:?}", v),
            };
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// File to patch
    filename: String,

    /// Disassemble the code
    #[arg(short, long)]
    disassemble: bool,
}

fn main() {
    let args = Args::parse();

    let mut vm = Vm::new();
    load_builtins(&mut vm);
    vm.load(&args.filename).unwrap();

    if args.disassemble {
        vm.disassemble_all();
        return;
    }

    let test = vm.variables.get("$self").unwrap();
    let Value::Class(test_class) = test else {
        panic!("LOL");
    };

    let test_class = test_class.clone();
    vm.stack.push(Value::Class(test_class.clone()));
    vm.call_method(&test_class.borrow(), "main", 0);
    vm.run();
}

fn read_string(f: &mut File) -> String {
    let size = f.read_u16::<LittleEndian>().unwrap() as usize;
    if size > 0 {
        let mut buf = vec![0u8; size];
        f.read(&mut buf).unwrap();

        String::from_utf8(buf).unwrap()
    } else {
        String::new()
    }
}

fn load_builtins(vm: &mut Vm) {
    load_system(vm);
    load_string(vm);
    load_list(vm);
}

macro_rules! generate_nl_writer {
    (true) => {
        println!("");
    };
    (false) => {
    };
}

macro_rules! generate_writer {
    ($nats:ident, $name:ident, $nl:tt, $args_count:literal) => {{
        let args_ph = if $args_count > 0 { format!("_{}", ",_".repeat($args_count - 1)) } else { "".to_string() };
        let name_ph = format!("{}({args_ph})", stringify!($name));

        $nats.insert(name_ph.clone(), NativeFunction {
            name: name_ph.clone(),
            arity: $args_count,
            code: |vm| {
                let base = vm.stack.len() - $args_count;
                for i in 0..$args_count {
                    let a = vm.stack.get(base + i).unwrap();
                    match a {
                        Value::Integer(i) => print!("{i}"),
                        Value::Float(f) => print!("{f}"),
                        Value::Bool(b) => print!("{b}"),
                        Value::String(s) => print!("{}", unescaper::unescape(s).unwrap()),
                        Value::Null => print!("null"),
                        Value::Object(o) => print!("<object '{}'>", o.borrow().class),
                        _ => print!("<error: {:?}>", a),
                    };
                }

                generate_nl_writer!($nl);
                vm.push_null();
            },
            //is_static: true,
        });
    }}
}

fn load_system(vm: &mut Vm) {
    let mut obj_nats = HashMap::new();
    obj_nats.insert("name".into(), NativeFunction {
        name: "name".into(),
        arity: 0,
        code: |vm| {
            let top = vm.stack.last().unwrap().clone();

            let Value::Class(class) = top else {
                panic!("Can't call name on not a class.");
            };

            vm.stack.push(Value::String(class.borrow().name.clone()));
        },
        //is_static: false,
    });
    obj_nats.insert("supertype".into(), NativeFunction {
        name: "supertype".into(),
        arity: 0,
        code: |vm| {
            let top = vm.stack.last().unwrap().clone();
            
            let Value::Class(class) = top else {
                panic!("Can't call supertype on not a class.");
            };

            if let Some(supertype) = &class.borrow().supertype {
                vm.stack.push(Value::String(supertype.clone()));
            } else {
                vm.push_null();
            };
        },
        //is_static: false,
    });
    let object_class = Class {
        name: "Object".into(),
        supertype: None,
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: obj_nats,
    };

    let mut nats = HashMap::new();

    generate_writer!(nats, print, true, 0);
    generate_writer!(nats, print, true, 1);
    generate_writer!(nats, print, true, 2);
    generate_writer!(nats, print, true, 3);
    generate_writer!(nats, print, true, 4);
    generate_writer!(nats, print, true, 5);
    generate_writer!(nats, print, true, 6);
    generate_writer!(nats, print, true, 7);
    generate_writer!(nats, print, true, 8);
    generate_writer!(nats, print, true, 9);
    generate_writer!(nats, print, true, 10);
    generate_writer!(nats, print, true, 11);
    generate_writer!(nats, print, true, 12);
    generate_writer!(nats, print, true, 13);
    generate_writer!(nats, print, true, 14);
    generate_writer!(nats, print, true, 15);
    generate_writer!(nats, print, true, 16);

    generate_writer!(nats, write, false, 1);
    generate_writer!(nats, write, false, 2);
    generate_writer!(nats, write, false, 3);
    generate_writer!(nats, write, false, 4);
    generate_writer!(nats, write, false, 5);
    generate_writer!(nats, write, false, 6);
    generate_writer!(nats, write, false, 7);
    generate_writer!(nats, write, false, 8);
    generate_writer!(nats, write, false, 9);
    generate_writer!(nats, write, false, 10);
    generate_writer!(nats, write, false, 11);
    generate_writer!(nats, write, false, 12);
    generate_writer!(nats, write, false, 13);
    generate_writer!(nats, write, false, 14);
    generate_writer!(nats, write, false, 15);
    generate_writer!(nats, write, false, 16);

    let system_class = Class {
        name: "System".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };

    vm.variables.insert("Object".into(), Value::Class(Rc::new(RefCell::new(object_class))));
    vm.variables.insert("System".into(), Value::Class(Rc::new(RefCell::new(system_class))));
}

fn load_string(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("count".into(), NativeFunction {
        name: "count".into(),
        arity: 0,
        code: |vm| {
            let top = vm.stack.last().unwrap();
            let Value::String(string) = top else {
                panic!("Can't call String.count on {top}.");
            };
            vm.stack.push(Value::Integer(string.len() as i32));
        },
        //is_static: false,
    });
    let string_class = Class {
        name: "String".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };

    vm.variables.insert("String".into(), Value::Class(Rc::new(RefCell::new(string_class))));
}

fn load_list(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("new()".into(), NativeFunction {
        name: "new()".into(),
        arity: 0,
        code: |vm| {
            vm.stack.push(Value::List(Vec::new()));
        },
        //is_static: true,
    });
    nats.insert("push(_)".into(), NativeFunction {
        name: "push(_)".into(),
        arity: 1,
        code: |vm| {
            let index = vm.stack.len() - 2;
            let arg = vm.stack.last().unwrap().clone();

            match vm.stack.get_mut(index).unwrap() {
                Value::List(ref mut list) => {
                    (*list).push(arg);
                },
                _ => todo!(),
            };

            vm.stack.push(vm.stack.get(index).unwrap().clone());
        },
        //is_static: false,
    });
    nats.insert("count".into(), NativeFunction {
        name: "count".into(),
        arity: 0,
        code: |vm| {
            let top = vm.stack.last().unwrap();
            let Value::List(list) = top else {
                panic!("Can't call List.count on {top}.");
            };
            vm.stack.push(Value::Integer(list.len() as i32));
        },
        //is_static: false,
    });
    let list_class = Class {
        name: "List".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };

    vm.variables.insert("List".into(), Value::Class(Rc::new(RefCell::new(list_class))));
}
