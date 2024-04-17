use std::cell::RefCell;
use std::rc::Rc;
use std::io::Read;
use std::fs::File;
use std::collections::HashMap;
use byteorder::{ReadBytesExt, LittleEndian};
use clap::Parser;

mod value;
use value::*;

mod core;

const VAL_NULL: u8 = 1;
const VAL_BOOL: u8 = 2;
const VAL_INTEGER: u8 = 3;
const VAL_FLOAT: u8 = 4;
const VAL_STRING: u8 = 5;

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
    Closure(Rc<str>),
    Pop,
    LoadModuleVar(Rc<str>),
    StoreModuleVar(Rc<str>),
    Call(Rc<str>, u8),
    Super(Rc<str>, u8),
    LoadLocalVar(usize),
    StoreLocalVar(usize),
    AllocateVar(Rc<str>),
    LoadFieldThis(Rc<str>),
    StoreFieldThis(Rc<str>),
    JumpIf(u8),
    Jump(u8),
    Loop(u8),
    LoopIf(u8),
    Dup,
    ImportModule(Rc<str>, HashMap<Rc<str>, Rc<str>>),

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
const OP_CLOSURE: u8 = 15;
const OP_POP: u8 = 16;
const OP_LOAD_MODULE_VAR: u8 = 17;
const OP_STORE_MODULE_VAR: u8 = 18;
const OP_CALL: u8 = 19;
const OP_LOOP: u8 = 20;
const OP_LOAD_LOCAL_VAR: u8 = 21;
const OP_STORE_LOCAL_VAR: u8 = 22;
const OP_ALLOCATE_VAR: u8 = 23;
const OP_LOAD_FIELD_THIS: u8 = 24;
const OP_STORE_FIELD_THIS: u8 = 25;
const OP_JUMP_IF: u8 = 26;
const OP_JUMP: u8 = 27;
const OP_DUP: u8 = 28;
const OP_LOOP_IF: u8 = 29;
const OP_IMPORT_MODULE: u8 = 30;
const OP_SUPER: u8 = 31;
const OP_DUMP_STACK: u8 = 255;

#[derive(Debug, Clone)]
#[allow(unused)]
struct Function {
    name: Rc<str>,
    arity: u8,
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
    name: Rc<str>,
    arity: u8,
    code: fn(&mut Vm),
}

#[derive(Debug, Clone)]
#[allow(unused)]
struct Fiber {
    upvalues: Vec<Value>,
    callstack: Vec<Function>,
    stackbase: Vec<usize>,
    stack: Vec<Value>,
    error: Option<Rc<str>>,
    trying: bool,
}

impl Fiber {
    fn new() -> Self {
        Self {
            upvalues: vec![],
            callstack: vec![],
            stackbase: vec![],
            stack: vec![],
            error: None,
            trying: false,
        }
    }

    fn push_callframe(&mut self, function: Function) {
        self.stackbase.push(self.stack.len() - function.arity as usize - 1);

        for _ in 0..function.locals {
            self.push_null();
        }

        self.callstack.push(function);
    }

    fn pop_callframe(&mut self) {
        self.callstack.pop().unwrap();
        self.stackbase.pop().unwrap();
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn push_null(&mut self) {
        self.push(Value::Null);
    }

    fn push_bool(&mut self, value: bool) {
        self.push(Value::Bool(value));
    }

    fn dump_stack(&self, name: &str) {
        println!("===== STACK {name} =====");
        let mut current = 0;
        for (i, s) in self.stack.iter().enumerate() {
            if current < self.stackbase.len() && self.stackbase[current] == i {
                print!("----- {} -----\n", self.callstack[current].name);
                current += 1;
            }

            let index = i - self.stackbase[current - 1];
            println!("{}/ {}", index, s);
        }
        println!("=====  END  {name} =====");
    }
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

#[derive(Debug)]
struct Vm {
    fibers: Vec<Rc<RefCell<Fiber>>>,
    variables: HashMap<Rc<str>, Value>,
    strings: Vec<Rc<str>>,
}

impl Vm {
    fn new() -> Self {
        Self {
            fibers: vec![Rc::new(RefCell::new(Fiber::new()))],
            variables: HashMap::new(),
            strings: vec![],
        }
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

    fn load(&mut self, filename: &str) -> std::io::Result<()> {
        let mut f = File::open(filename)?;
        let magic = f.read_u32::<LittleEndian>()?;
        if magic != 0x4B434F52 {
            panic!("Wrong magic bytes.");
        }

        let version = f.read_u8()?; assert_eq!(version, 1);

        let strings_len = f.read_u32::<LittleEndian>()?;
        for _ in 0..strings_len {
            self.strings.push(read_string(&mut f));
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

                    default.push(self.decode(&name, c, &mut buf, &mut i)?);
                }

                fields.insert(name, default);
            }

            let functions_len = f.read_u8()?;

            let mut functions = HashMap::new();
            for _ in 0..functions_len {
                let name = read_string(&mut f);
                let arity = f.read_u8()?;
                let locals = f.read_u8()?;
                
                let size = f.read_u16::<LittleEndian>()? as usize;
                let mut buf = vec![0u8; size];
                f.read(&mut buf).unwrap();
                let mut code = Chunk::new();

                let mut i = 0;
                while i < buf.len() {
                    let c = buf[i];
                    i += 1;

                    code.code.push(self.decode(&name, c, &mut buf, &mut i)?);
                }

                functions.insert(name.clone(), Function {
                    name: name.clone(), arity, locals, code,
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

    fn decode(&self, name: &str, c: u8, buf: &mut Vec<u8>, i: &mut usize) -> std::io::Result<Opcode> {
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
                        Value::String(self.strings[int].clone())
                    },
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
            OP_CLOSURE => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::Closure(self.strings[idx].clone())
            },
            OP_POP => Opcode::Pop,
            OP_LOAD_MODULE_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::LoadModuleVar(self.strings[idx].clone())
            },
            OP_STORE_MODULE_VAR => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::StoreModuleVar(self.strings[idx].clone())
            },
            OP_CALL => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                *i += 1;
                let args = buf[*i - 1];
                Opcode::Call(self.strings[idx].clone(), args)
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
                Opcode::AllocateVar(self.strings[idx].clone())
            },
            OP_LOAD_FIELD_THIS => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::LoadFieldThis(self.strings[idx].clone())
            },
            OP_STORE_FIELD_THIS => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                Opcode::StoreFieldThis(self.strings[idx].clone())
            },
            OP_JUMP_IF => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_u8()?;
                Opcode::JumpIf(offset)
            },
            OP_JUMP => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_u8()?;
                Opcode::Jump(offset)
            },
            OP_LOOP => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_u8()?;
                Opcode::Loop(offset)
            },
            OP_LOOP_IF => {
                *i += 1;
                let offset = (&buf[(*i - 1)..*i]).read_u8()?;
                Opcode::LoopIf(offset)
            },
            OP_IMPORT_MODULE => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;

                let mut imports = HashMap::new();

                *i += 2;
                let count = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                for _ in 0..count {
                    *i += 2;
                    let key = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                    *i += 2;
                    let val = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;

                    imports.insert(self.strings[key].clone(), self.strings[val].clone());
                }

                Opcode::ImportModule(self.strings[idx].clone(), imports)
            },
            OP_DUP => Opcode::Dup,
            OP_SUPER => {
                *i += 2;
                let idx = (&buf[(*i - 2)..*i]).read_u16::<LittleEndian>()? as usize;
                *i += 1;
                let args = buf[*i - 1];
                Opcode::Super(self.strings[idx].clone(), args)
            },
            _ => {
                println!("{:?}", buf);
                panic!("{name}: Unknown byte {:#X} at index {}", c, *i - 1);
            },
        };

        Ok(ret)
    }

    fn run(&mut self) {
        'mainloop: loop {
            let mut fiber = self.fibers.last_mut().unwrap().borrow_mut();
            if let Some(msg) = &fiber.error {
                let msg = msg.clone();

                while !fiber.trying {
                    drop(fiber);

                    self.fibers.pop();
                    if self.fibers.len() == 0 {
                        panic!("A error as occured: {msg}");
                    } else {
                        fiber = self.fibers.last_mut().unwrap().borrow_mut();
                    }
                }

                drop(fiber);
                self.fibers.pop();
                fiber = self.fibers.last_mut().unwrap().borrow_mut();
                fiber.push(Value::String(msg));
            }

            let func = fiber.callstack.last_mut().unwrap();
            func.code.ip += 1;
            let ip = func.code.ip - 1;
            let inst = func.code.code[ip].clone();
            let stackbase = fiber.stackbase.last().unwrap().clone();

            match inst {
                Opcode::Return => {
                    let ret = fiber.pop();
                    while fiber.stack.len() > stackbase {
                        fiber.pop();
                    }
                    fiber.push(ret);

                    fiber.pop_callframe();
                    if fiber.callstack.len() == 0 {
                        assert_eq!(fiber.stack.len(), 1);
                        drop(fiber);
                        if self.fibers.len() > 1 {
                            let fiber = self.fibers.last().unwrap().borrow();
                            let ret = fiber.stack.last().unwrap().clone();
                            drop(fiber);
                            self.fibers.pop();
                            let mut fiber = self.fibers.last_mut().unwrap().borrow_mut();
                            fiber.push(ret);
                        } else {
                            return;
                        }
                    }
                },
                Opcode::Constant(value) => fiber.push(value.clone()),
                Opcode::Negate => {
                    let val = fiber.pop();
                    fiber.push(-val);
                },
                Opcode::Add => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push(l + r);
                },
                Opcode::Sub => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push(l - r);
                },
                Opcode::Mul => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push(l * r);
                },
                Opcode::Div => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push(l / r);
                },
                Opcode::True => fiber.push_bool(true),
                Opcode::False => fiber.push_bool(false),
                Opcode::Null => fiber.push_null(),
                Opcode::Not => {
                    let val = fiber.pop();
                    fiber.push(!val);
                },
                Opcode::Equal => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push_bool(l == r);
                },
                Opcode::LowerThan => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push_bool(l < r);
                },
                Opcode::GreaterThan => {
                    let r = fiber.pop();
                    let l = fiber.pop();
                    fiber.push_bool(l > r);
                },
                Opcode::Closure(name) => {
                    let val = fiber.pop();
                    let Value::Class(class) = val else {
                        fiber.error = Some(format!("{:?} => {name}", val).into());
                        continue;
                    };

                    if let Some(func) = class.borrow().functions.get(&name) {
                        fiber.push(Value::Closure(func.clone()));
                    } else {
                        fiber.error = Some(format!("{} does not have a function named {name}", class.borrow().name).into());
                    };
                },
                Opcode::Pop => {
                    fiber.pop();
                },
                Opcode::LoadModuleVar(name) => {
                    let var = match self.variables.get(&name) {
                        Some(val) => val,
                        None => {
                            fiber.error = Some(format!("Unknown variable '{}'.", name).into());
                            continue;
                        },
                    };
                    fiber.push(var.clone());
                },
                Opcode::Call(name, args) => {
                    let name = name.clone();
                    let index = fiber.stack.len() - (args as usize) - 1;
                    let top = fiber.stack[index].clone();

                    let class = match top {
                        Value::Class(c) => c,
                        Value::Object(o) => {
                            let Some(Value::Class(c)) = self.variables.get(&o.borrow().class) else {
                                fiber.error = Some(format!("Object of unknown class: {}", o.borrow().class).into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::String(_) => {
                            let Some(Value::Class(c)) = self.variables.get("String") else {
                                fiber.error = Some("Can't find class 'String'.".into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::List(_) => {
                            let Some(Value::Class(c)) = self.variables.get("List") else {
                                fiber.error = Some("Can't find class 'List'.".into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::Fiber(_) => {
                            let Some(Value::Class(c)) = self.variables.get("Fiber") else {
                                fiber.error = Some("Can't find class 'Fiber'.".into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::Bool(_) => {
                            let Some(Value::Class(c)) = self.variables.get("Bool") else {
                                fiber.error = Some("Can't find class 'Bool'.".into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::Closure(_) => {
                            let Some(Value::Class(c)) = self.variables.get("Fn") else {
                                fiber.error = Some("Can't find class 'Fn'.".into());
                                continue;
                            };

                            c.clone()
                        },
                        Value::Pointer(p) => {
                            let Some(Value::Class(c)) = self.variables.get(&p.borrow().class) else {
                                fiber.error = Some(format!("Pointer of unknown class: {}", p.borrow().class).into());
                                continue;
                            };

                            c.clone()
                        },
                        _ => {
                            fiber.error = Some(format!("Calling '{name}' with {:?}.", top).into());
                            continue;
                        },
                    };

                    drop(fiber);
                    let err = self.call_method(&class.borrow(), &name, args, &class.borrow().name);
                    let mut fiber = self.fibers.last_mut().unwrap().borrow_mut();
                    if fiber.error.is_none() && err.is_some() {
                        fiber.error = err;
                    }
                },
                Opcode::Super(name, args) => {
                    let name = name.clone();
                    let index = fiber.stack.len() - (args as usize) - 1;
                    let top = fiber.stack[index].clone();

                    let class = match top {
                        Value::Object(o) => {
                            let Some(Value::Class(c)) = self.variables.get(&o.borrow().class) else {
                                fiber.error = Some(format!("Object of unknown class: {}", o.borrow().class).into());
                                continue;
                            };

                            let Some(supertype) = &c.borrow().supertype else {
                                fiber.error = Some(format!("Class '{}' doesn't have a parent class.", c.borrow().name).into());
                                continue;
                            };

                            let Some(Value::Class(pc)) = self.variables.get(supertype) else {
                                fiber.error = Some(format!("Object of unknown class: {}", supertype).into());
                                continue;
                            };

                            pc.clone()
                        },
                        _ => {
                            fiber.error = Some(format!("Calling '{name}' in parent class with {:?}.", top).into());
                            continue;
                        },
                    };

                    drop(fiber);
                    let err = self.call_method(&class.borrow(), &name, args, &class.borrow().name);
                    let mut fiber = self.fibers.last_mut().unwrap().borrow_mut();
                    if fiber.error.is_none() && err.is_some() {
                        fiber.error = err;
                    }
                },
                Opcode::LoadLocalVar(index) => {
                    let idx = stackbase + index as usize;
                    let var = fiber.stack[idx].clone();
                    fiber.push(var);
                },
                Opcode::StoreLocalVar(index) => {
                    let value = fiber.pop();
                    let idx = stackbase + index as usize;
                    fiber.stack[idx] = value;
                    fiber.push_null();
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
                                    fiber.error = Some(format!("'{name}' is not a class.").into());
                                    continue 'mainloop;
                                }
                            },
                            None => {
                                fiber.error = Some(format!("Unknown variable '{}'.", name).into());
                                continue 'mainloop;
                            },
                        };

                        for (name, default) in &class.borrow().fields {
                            let value = || -> Value {
                                let mut id = 0;
                                let mut stack = vec![];
                                while id < default.len() {
                                    let op = &default[id];
                                    match op {
                                        Opcode::Constant(c) => {
                                            stack.push(c);
                                        },
                                        _ => {
                                            fiber.error = Some(format!("{:?} is not allowed in field initialiser.", op).into());
                                            return Value::Null;
                                        },
                                    };
                                    id += 1;
                                }

                                assert_eq!(stack.len(), 1);

                                stack[0].clone()
                            }();

                            if fiber.error.is_some() {
                                continue 'mainloop;
                            }

                            fields.insert(name.clone(), value);
                        }

                        if let Some(parent) = &class.borrow().supertype {
                            parent_name = parent.clone();
                        } else {
                            parent_name = "".into();
                        }
                    }

                    fiber.push(Value::Object(Rc::new(RefCell::new(Object {
                        class: name,
                        fields,
                    }))));
                },
                Opcode::StoreModuleVar(name) => {
                    let var = fiber.pop();
                    self.variables.insert(name, var);
                    fiber.push_null();
                },
                Opcode::LoadFieldThis(name) => {
                    let val = &fiber.stack[stackbase];
                    let Value::Object(obj) = val else {
                        fiber.error = Some(format!("{:?} => {name}", val).into());
                        continue;
                    };

                    let var = obj.borrow().fields.get(&name).unwrap().clone();
                    fiber.push(var);
                },
                Opcode::StoreFieldThis(name) => {
                    let var = fiber.pop();
                    let val = fiber.stack.get_mut(stackbase).unwrap();
                    let Value::Object(ref mut obj) = val else {
                        fiber.error = Some(format!("{:?} => {name}", val).into());
                        continue;
                    };

                    if let Some(field) = obj.borrow_mut().fields.get_mut(&name) {
                        *field = var.clone();
                    }

                    fiber.push(var);
                },
                Opcode::Loop(offset) => {
                    let func = fiber.callstack.last_mut().unwrap();
                    // "ip" is past loop, so + 2 to place it before loop.
                    func.code.ip -= (offset + 2) as usize;
                },
                Opcode::Jump(offset) => {
                    let func = fiber.callstack.last_mut().unwrap();
                    func.code.ip += offset as usize;
                },
                Opcode::JumpIf(offset) => {
                    let var = fiber.pop();
                    let cond = match var {
                        Value::Bool(b) => b,
                        Value::Integer(i) => i != 0,
                        _ => {
                            fiber.error = Some("JumpIf: false".into());
                            continue 'mainloop;
                        },
                    };

                    if cond {
                        let func = fiber.callstack.last_mut().unwrap();
                        let newip = (func.code.ip as i32 + offset as i32) as usize;
                        func.code.ip = newip;
                    }
                },
                Opcode::LoopIf(offset) => {
                    let var = fiber.pop();
                    let cond = match var {
                        Value::Bool(b) => b,
                        Value::Integer(i) => i != 0,
                        _ => {
                            fiber.error = Some("LoopIf: false".into());
                            continue 'mainloop;
                        },
                    };

                    if cond {
                        let func = fiber.callstack.last_mut().unwrap();
                        let newip = (func.code.ip as i32 - offset as i32) as usize;
                        func.code.ip = newip;
                    }
                },
                Opcode::Dup => {
                    let var = fiber.stack.last().unwrap().clone();
                    fiber.push(var);
                },
                Opcode::ImportModule(name, imports) => {
                    let fname = format!("{name}.rock");
                    let vm = execute_script(&fname, false).unwrap();
                    for (name, value) in vm.variables {
                        if imports.contains_key(&name) {
                            self.variables.insert(imports.get(&name).unwrap().clone(), value);
                        }
                    }
                },
                Opcode::DumpStack => {
                    fiber.dump_stack(&format!("opcode [ip: {:#X}]", ip));
                },
            };
        }
    }

    fn call_method(&mut self, class: &Class, name: &str, args: u8, orig_class: &str) -> Option<Rc<str>> {
        if let Some(func) = class.functions.get(name) {
            return self.call(func, args);
        }

        if let Some(func) = class.natives.get(name) {
            return self.call_native(func, args);
        }

        if let Some(supertype_name) = &class.supertype {
            let Some(supertype) = self.variables.get(supertype_name) else {
                return Some(format!("'{}' not found.", supertype_name).into());
            };
            let Value::Class(supertype_class) = supertype else {
                return Some(format!("'{}' is not a class.", supertype_name).into());
            };

            let supertype_class = supertype_class.clone();
            return self.call_method(&supertype_class.borrow(), name, args, orig_class);
        }

        Some(format!("'{}' doesn't have function named '{}'.", orig_class, name).into())
    }

    fn call(&mut self, func: &Function, args: u8) -> Option<Rc<str>> {
        if args != func.arity {
            return Some(format!("{} call: {} vs {}", func.name, func.arity, args).into());
        }

        let mut fiber = self.fibers.last_mut().unwrap().borrow_mut();
        fiber.push_callframe(func.clone());

        None
    }

    fn call_native(&mut self, func: &NativeFunction, args: u8) -> Option<Rc<str>> {
        if args != func.arity {
            return Some(format!("{} call: {} vs {}", func.name, func.arity, args).into());
        }

        (func.code)(self);

        None
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
    execute_script(&args.filename, args.disassemble);
}

fn execute_script(filename: &str, disassemble: bool) -> Option<Vm> {
    let mut vm = Vm::new();
    core::register(&mut vm);
    vm.load(&filename).unwrap();

    if disassemble {
        vm.disassemble_all();
        return None;
    }

    let test = vm.variables.get("$self").unwrap();
    let Value::Class(test_class) = test else {
        panic!("LOL");
    };

    let test_class = test_class.clone();
    vm.fibers.last_mut().unwrap().borrow_mut().push(Value::Class(test_class.clone()));
    vm.call_method(&test_class.borrow(), "main", 0, &test_class.borrow().name);
    vm.run();

    Some(vm)
}

fn read_string(f: &mut File) -> Rc<str> {
    let size = f.read_u16::<LittleEndian>().unwrap() as usize;
    if size > 0 {
        let mut buf = vec![0u8; size];
        f.read(&mut buf).unwrap();

        String::from_utf8(buf).unwrap().into()
    } else {
        String::new().into()
    }
}
