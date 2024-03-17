use std::io::Read;
use crate::Pointer;
use std::fs::File;
use crate::Function;
use crate::Fiber;
use crate::Chunk;
use crate::Opcode;
use std::cell::RefCell;
use std::rc::Rc;
use crate::Class;
use std::collections::HashMap;
use crate::NativeFunction;
use crate::Value;
use crate::Vm;

pub fn register(vm: &mut Vm) {
    load_system(vm);
    load_sequence(vm);
    load_bool(vm);
    load_string(vm);
    load_list(vm);
    load_fn(vm);
    load_fiber(vm);
    load_file(vm);
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
        let name_ph: Rc<str> = format!("{}({args_ph})", stringify!($name)).into();

        $nats.insert(name_ph.clone(), NativeFunction {
            name: name_ph.clone(),
            arity: $args_count,
            code: |vm| {
                let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
                let base = fiber.stack.len() - $args_count;
                for i in 0..$args_count {
                    let a = fiber.stack.get(base + i).unwrap();
                    print!("{a}");
                }

                generate_nl_writer!($nl);

                for _ in 0..$args_count {
                    fiber.pop();
                }
                fiber.pop();
                fiber.push_null();
            },
        });
    }}
}

fn load_system(vm: &mut Vm) {
    let mut obj_nats = HashMap::new();
    obj_nats.insert("name".into(), NativeFunction {
        name: "name".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();

            let Value::Class(class) = top else {
                fiber.error = Some(format!("Can't call name on {:?}.", top).into());
                fiber.push_null();
                return;
            };

            fiber.pop();
            fiber.stack.push(Value::String(class.borrow().name.clone()));
        },
    });
    obj_nats.insert("supertype".into(), NativeFunction {
        name: "supertype".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            
            let Value::Class(class) = top else {
                fiber.error = Some(format!("Can't call supertype on {:?}.", top).into());
                fiber.push_null();
                return;
            };

            fiber.pop();
            if let Some(supertype) = &class.borrow().supertype {
                fiber.stack.push(Value::String(supertype.clone()));
            } else {
                fiber.push_null();
            };
        },
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

fn load_bool(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("!".into(), NativeFunction {
        name: "!".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::Bool(b) = top else {
                fiber.error = Some(format!("Can't call ! on {:?}.", top).into());
                fiber.push_null();
                return;
            };

            fiber.stack.push(Value::Bool(!b));
        },
    });
    let bool_class = Class {
        name: "Bool".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };

    vm.variables.insert("Bool".into(), Value::Class(Rc::new(RefCell::new(bool_class))));
}

fn load_string(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("count".into(), NativeFunction {
        name: "count".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::String(string) = top else {
                fiber.error = Some(format!("Can't call String.count on {:?}.", top).into());
                fiber.push_null();
                return;
            };

            fiber.stack.push(Value::Integer(string.len() as i32));
        },
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

fn load_sequence(vm: &mut Vm) {
    let mut funcs = HashMap::new();

    let mut code = Chunk::new();
    code.code.push(Opcode::LoadLocalVar(0));
    code.code.push(Opcode::LoadLocalVar(2));
    code.code.push(Opcode::Call("iterate(_)".into(), 1));
    code.code.push(Opcode::StoreLocalVar(2));
    code.code.push(Opcode::Pop);
    code.code.push(Opcode::LoadLocalVar(2));
    code.code.push(Opcode::Not);
    code.code.push(Opcode::JumpIf(7));
    code.code.push(Opcode::LoadLocalVar(1));
    code.code.push(Opcode::LoadLocalVar(0));
    code.code.push(Opcode::LoadLocalVar(2));
    code.code.push(Opcode::Call("iteratorValue(_)".into(), 1));
    code.code.push(Opcode::Call("call(_)".into(), 1));
    code.code.push(Opcode::Pop);
    code.code.push(Opcode::Loop(13));
    code.code.push(Opcode::Null);
    code.code.push(Opcode::Return);

    funcs.insert("each(_)".into(), Function {
        name: "each(_)".into(),
        arity: 1,
        locals: 1,
        code,
    });

    let nats = HashMap::new();

    let sequence_class = Class {
        name: "Sequence".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: funcs,
        natives: nats,
    };

    vm.variables.insert("Sequence".into(), Value::Class(Rc::new(RefCell::new(sequence_class))));
}

fn load_list(vm: &mut Vm) {
    let funcs = HashMap::new();

    let mut nats = HashMap::new();
    nats.insert("new()".into(), NativeFunction {
        name: "new()".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            fiber.stack.pop();
            fiber.stack.push(Value::List(Rc::new(RefCell::new(Vec::new()))));
        },
    });

    nats.insert("add(_)".into(), NativeFunction {
        name: "add(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let index = fiber.stack.len() - 2;
            let arg = fiber.stack.pop().unwrap();

            match fiber.stack.get_mut(index).unwrap() {
                Value::List(list) => {
                    list.borrow_mut().push(arg);
                },
                _ => todo!(),
            };

            // keep the list at the top of the stack
        },
    });

    nats.insert("iterate(_)".into(), NativeFunction {
        name: "iterate(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let arg = fiber.pop();
            let list = fiber.pop();

            let Value::List(list) = list else {
                todo!();
            };

            let next = match arg {
                Value::Null => 0,
                Value::Integer(i) => i + 1,
                _ => panic!("debug: {:?}", arg),
            };

            if next < 0 {
                panic!("no negative iterator");
            }

            if (next as usize) < list.borrow().len() {
                fiber.stack.push(Value::Integer(next));
            } else {
                fiber.push_bool(false);
            }
        },
    });

    nats.insert("iteratorValue(_)".into(), NativeFunction {
        name: "iteratorValue(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let arg = fiber.pop();
            let list = fiber.pop();

            let Value::List(list) = list else {
                todo!();
            };

            let Value::Integer(index) = arg else {
                todo!();
            };

            if index < 0 {
                panic!("no negative iterator");
            }

            let index = index as usize;

            if index < list.borrow().len() {
                let val = list.borrow_mut()[index].clone();
                fiber.stack.push(val);
            } else {
                fiber.push_null();
            }
        },
    });

    nats.insert("[_]".into(), NativeFunction {
        name: "[_]".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let arg = fiber.pop();
            let list = fiber.pop();

            let Value::List(list) = list else {
                todo!();
            };

            let Value::Integer(index) = arg else {
                todo!();
            };

            if index < 0 {
                panic!("no negative iterator");
            }

            let index = index as usize;

            if index < list.borrow().len() {
                let val = list.borrow()[index].clone();
                fiber.stack.push(val);
            } else {
                fiber.push_null();
            }
        },
    });

    nats.insert("[_]=(_)".into(), NativeFunction {
        name: "[_]=(_)".into(),
        arity: 2,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let val = fiber.pop();
            let index = fiber.pop();
            let list = fiber.pop();

            let Value::List(list) = list else {
                todo!();
            };

            let Value::Integer(index) = index else {
                todo!();
            };

            if index < 0 {
                panic!("no negative iterator");
            }

            let index = index as usize;

            if index < list.borrow().len() {
                list.borrow_mut()[index] = val.clone();
                fiber.stack.push(val);
            } else {
                fiber.push_null();
            }
        },
    });

    nats.insert("count".into(), NativeFunction {
        name: "count".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::List(list) = top else {
                fiber.error = Some(format!("Can't call List.count on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            fiber.stack.push(Value::Integer(list.borrow().len() as i32));
        },
    });

    let list_class = Class {
        name: "List".into(),
        supertype: Some("Sequence".into()),
        fields: HashMap::new(),
        functions: funcs,
        natives: nats,
    };

    vm.variables.insert("List".into(), Value::Class(Rc::new(RefCell::new(list_class))));
}

fn load_fn(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("new(_)".into(), NativeFunction {
        name: "new(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.last().unwrap();

            match top {
                Value::Closure(_) => {},
                _ => {
                    fiber.error = Some(format!("Fn.new(_) only accepts functions. Got {:?}", top).into());
                    fiber.push_null();
                    return;
                },
            };
        },
    });

    nats.insert("call(_)".into(), NativeFunction {
        name: "call(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();

            let function = match fiber.stack.pop().unwrap() {
                Value::Closure(function) => function,
                _ => {
                    fiber.error = Some(format!("Wanted 'Fn', got {:?}.", top).into());
                    fiber.push_null();
                    return;
                },
            };

            let len = fiber.stack.len();
            fiber.push_null();
            fiber.stack.push(top);
            fiber.stackbase.push(len);
            fiber.callstack.push(function);
        },
    });

    nats.insert("call()".into(), NativeFunction {
        name: "call()".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();

            let function = match top {
                Value::Closure(function) => function,
                _ => {
                    fiber.error = Some(format!("Wanted 'Fn', got {:?}.", top).into());
                    fiber.push_null();
                    return;
                },
            };

            let len = fiber.stack.len();
            fiber.stackbase.push(len - 1);
            fiber.callstack.push(function);
        },
    });

    let fn_class = Class {
        name: "Fn".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };
    vm.variables.insert("Fn".into(), Value::Class(Rc::new(RefCell::new(fn_class))));
}

fn load_fiber(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("new(_)".into(), NativeFunction {
        name: "new(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::Closure(function) = top else {
                fiber.error = Some(format!("Can't call Fiber.new(_) on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            fiber.pop(); // pops "Fiber" class

            let mut new_fiber = Fiber::new();
            new_fiber.push(Value::Closure(function.clone()));
            new_fiber.push_callframe(function.clone());
            fiber.push(Value::Fiber(Rc::new(RefCell::new(new_fiber))));
        },
    });

    nats.insert("call(_)".into(), NativeFunction {
        name: "call(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let val = fiber.pop();
            let top = fiber.pop();

            let Value::Fiber(new_fiber) = top else {
                fiber.error = Some(format!("Can't call Fiber.call(_) on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            drop(fiber);

            new_fiber.borrow_mut().push(val);
            vm.fibers.push(new_fiber);
        },
    });

    nats.insert("call()".into(), NativeFunction {
        name: "call()".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.pop();

            let Value::Fiber(new_fiber) = top else {
                fiber.error = Some(format!("Can't call Fiber.call() on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            drop(fiber);

            new_fiber.borrow_mut().push_null();
            vm.fibers.push(new_fiber);
        },
    });

    nats.insert("try(_)".into(), NativeFunction {
        name: "try(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let val = fiber.pop();
            let top = fiber.pop();

            let Value::Fiber(new_fiber) = top else {
                fiber.error = Some(format!("Can't call Fiber.try(_) on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            drop(fiber);

            new_fiber.borrow_mut().push(val);
            new_fiber.borrow_mut().trying = true;
            vm.fibers.push(new_fiber);
        },
    });

    nats.insert("try()".into(), NativeFunction {
        name: "try()".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.pop();

            let Value::Fiber(new_fiber) = top else {
                fiber.error = Some(format!("Can't call Fiber.try() on {:?}.", top).into());
                fiber.push_null();
                return;
            };
            drop(fiber);

            new_fiber.borrow_mut().push_null();
            new_fiber.borrow_mut().trying = true;
            vm.fibers.push(new_fiber);
        },
    });

    nats.insert("yield(_)".into(), NativeFunction {
        name: "yield(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.pop();
            fiber.pop();
            drop(fiber);

            vm.fibers.pop();

            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            fiber.push(top);
        },
    });

    nats.insert("abort(_)".into(), NativeFunction {
        name: "abort(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.pop();
            fiber.pop();

            match top {
                Value::Null => {},
                Value::String(s) => fiber.error = Some(s),
                _ => {
                    fiber.error = Some(format!("Invalid value given to Fiber.abort(_). Null or a string. Got {:?}.", top).into());
                    fiber.push_null();
                    return;
                }
            };
            
            fiber.push_null();
        },
    });

    nats.insert("isDone".into(), NativeFunction {
        name: "isDone".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.pop();
            let Value::Fiber(new_fiber) = top else {
                fiber.error = Some(format!("Fiber.isDone expects a fiber. Got {:?}.", top).into());
                fiber.push_null();
                return;
            };

            if new_fiber.borrow().callstack.len() == 0 {
                fiber.push_bool(true);
            } else {
                fiber.push_bool(false);
            }
        },
    });

    let fiber_class = Class {
        name: "Fiber".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };
    vm.variables.insert("Fiber".into(), Value::Class(Rc::new(RefCell::new(fiber_class))));
}

fn load_file(vm: &mut Vm) {
    let mut nats = HashMap::new();
    nats.insert("new(_)".into(), NativeFunction {
        name: "new(_)".into(),
        arity: 1,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::String(filename) = top else {
                fiber.error = Some(format!("File.new(_) expects a string. Got {:?}.", top).into());
                fiber.push_null();
                return;
            };
            
            fiber.pop(); // pops "File" class

            let file = File::open(&*filename).unwrap();
            fiber.stack.push(Value::Pointer(Rc::new(RefCell::new(Pointer {
                class: "File".into(),
                data: Box::new(file),
            }))));
        },
    });
    nats.insert("read()".into(), NativeFunction {
        name: "read()".into(),
        arity: 0,
        code: |vm| {
            let mut fiber = vm.fibers.last_mut().unwrap().borrow_mut();
            let top = fiber.stack.pop().unwrap();
            let Value::Pointer(ptr) = top else {
                fiber.error = Some(format!("File.read() expects a pointer. Got {:?}.", top).into());
                fiber.push_null();
                return;
            };

            if let Some(mut file) = ptr.borrow().data.downcast_ref::<File>() {
                let mut buffer = String::new();
                file.read_to_string(&mut buffer).unwrap();
                fiber.push(Value::String(Rc::from(buffer)));
            } else {
                fiber.error = Some("File.read() expects a 'File'.".into());
                fiber.push_null();
            };
        },
    });

    let file_class = Class {
        name: "File".into(),
        supertype: Some("Object".into()),
        fields: HashMap::new(),
        functions: HashMap::new(),
        natives: nats,
    };
    vm.variables.insert("File".into(), Value::Class(Rc::new(RefCell::new(file_class))));
}
