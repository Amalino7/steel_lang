use crate::typechecker::types::{FunctionType, Type};
use crate::typechecker::Symbol;
use crate::vm::value::{NativeFn, Value};
use std::rc::Rc;

pub struct NativeDef {
    pub name: &'static str,
    pub type_: Type,
    pub func: NativeFn,
}

fn instance_method(
    params: Vec<(String, Type)>,
    return_type: Type,
    type_params: Vec<Symbol>,
) -> Type {
    Type::Function(Rc::new(FunctionType {
        is_static: false,
        is_vararg: false,
        params,
        return_type,
        type_params,
    }))
}

pub fn get_prelude() -> &'static str {
    // standard logic lives here
    r#"
        impl number {
            func abs(self): number {
                if self < 0 {
                    return -self;
                }
                else{
                    return self;
                }
            }
        }
        func Void() {}
        enum Result<Ok, Err> {
            Ok(Ok),Err(Err)
        }
        impl<Ok,Err> Result<Ok, Err> {
            func unwrap(self): Ok {
                match self {
                    .Ok(val) => {return val;}
                    .Err(err) => {return panic(err);}
                }
            }
            func map<NewOk>(self, f: func(Ok): NewOk): Result<NewOk, Err> {
                match self {
                    .Ok(val) => {return Result.Ok(f(val));}
                    .Err(err) => {return Result.Err(err);}
                }
            }
        }
        impl List<number> {
            func sum(self): number {
                func add(a: number, b: number): number {return a + b;}
                return self.fold(0, add);
            }
        }
        impl<T> List<T> {
            func contains(self, value: T): boolean {
                let i = 0;
                let contains = false;
                while i < self.len() {
                    contains = contains or (self[i] == value);
                    i+=1;
                }
                return contains;
            }
            func fold<U>(self, initial: U, f: func(U, T): U): U {
                let result = initial;
                let i = 0;
                while i < self.len() {
                    result = f(result, self[i]);
                    i+=1;
                }
                return result;
            }
            func each(self, f: func(T)): void {
                let i = 0;
                while i < self.len() {
                    f(self[i]);
                    i += 1;
                }
            }
            func map<U>(self, f: func(T): U): List<U> {
                let out: List<U> = [];
                let i = 0;
                while i < self.len() {
                    out.push(f(self[i]));
                    i += 1;
                }
                return out;
            }
            func filter(self, f: func(T): boolean): List<T> {
                let out: List<T> = [];
                let i = 0;
                while i < self.len() {
                    if f(self[i]) {
                        out.push(self[i]);
                    }
                    i += 1;
                }
                return out;
            }
        }
    "#
}
pub fn get_natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "println",
            type_: Type::new_vararg(vec![Type::Any], Type::Void),
            func: |args, _| {
                for arg in args {
                    print!("{}", arg);
                }
                println!();
                Ok(Value::Nil)
            },
        },
        NativeDef {
            name: "panic",
            type_: Type::new_function(vec![("msg".into(), Type::Any)], Type::Never, vec![]),
            func: |args, _| Err(args[0].to_string()),
        },
        NativeDef {
            name: "print",
            type_: Type::new_vararg(vec![Type::Any], Type::Void),
            func: |args, _| {
                for arg in args {
                    print!("{}", arg);
                }
                Ok(Value::Nil)
            },
        },
        NativeDef {
            name: "assert",
            type_: Type::new_function(
                vec![("left".into(), Type::Any), ("right".into(), Type::Any)],
                Type::Void,
                vec![],
            ),
            func: |args, _| {
                if args[0] != args[1] {
                    panic!("Assertion failed: {} != {}", args[0], args[1]);
                }
                Ok(Value::Nil)
            },
        },
        NativeDef {
            name: "clock",
            type_: Type::new_function(vec![], Type::Number, vec![]),
            func: |_, _| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let start = SystemTime::now();
                let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();
                Ok(Value::Number(since_the_epoch.as_secs_f64()))
            },
        },
        NativeDef {
            name: "to_str",
            type_: Type::new_function(
                vec![("_".into(), Type::GenericParam("T".into()))],
                Type::String,
                vec!["T".into()],
            ),
            func: |args, gc| {
                let str = args[0].to_string();
                Ok(Value::String(gc.alloc(str)))
            },
        },
        NativeDef {
            name: "is_nan",
            type_: Type::new_function(vec![("_".into(), Type::Number)], Type::Boolean, vec![]),
            func: |args, _| match args[0] {
                Value::Number(num) => Ok(Value::Boolean(num.is_nan())),
                _ => unreachable!(),
            },
        },
        NativeDef {
            name: "List.len",
            type_: instance_method(
                vec![(
                    "self".into(),
                    Type::List(Box::new(Type::GenericParam("T".into()))),
                )],
                Type::Number,
                vec!["T".into()],
            ),
            func: |args, _| match args[0] {
                Value::List(list) => Ok(Value::Number(list.vec.len() as f64)),
                _ => unreachable!(),
            },
        },
        NativeDef {
            name: "List.is_empty",
            type_: instance_method(
                vec![(
                    "self".into(),
                    Type::List(Box::new(Type::GenericParam("T".into()))),
                )],
                Type::Boolean,
                vec!["T".into()],
            ),
            func: |args, _| match args[0] {
                Value::List(list) => Ok(Value::Boolean(list.vec.is_empty())),
                _ => unreachable!(),
            },
        },
        NativeDef {
            name: "List.push",
            type_: instance_method(
                vec![
                    (
                        "self".into(),
                        Type::List(Box::new(Type::GenericParam("T".into()))),
                    ),
                    ("value".into(), Type::GenericParam("T".into())),
                ],
                Type::Void,
                vec!["T".into()],
            ),
            func: |args, _| match args[0] {
                Value::List(mut list) => unsafe {
                    list.deref_mut().vec.push(args[1]);
                    Ok(Value::Nil)
                },
                _ => unreachable!(),
            },
        },
        NativeDef {
            name: "List.pop",
            type_: instance_method(
                vec![(
                    "self".into(),
                    Type::List(Box::new(Type::GenericParam("T".into()))),
                )],
                Type::Optional(Box::new(Type::GenericParam("T".into()))),
                vec!["T".into()],
            ),
            func: |args, _| match args[0] {
                Value::List(mut list) => unsafe {
                    Ok(list.deref_mut().vec.pop().unwrap_or(Value::Nil))
                },
                _ => unreachable!(),
            },
        },
    ]
}

#[allow(dead_code)]
pub fn debug_size<T>() {
    println!(
        "Size of {}: {} bytes, alignment: {} bytes",
        std::any::type_name::<T>(),
        std::mem::size_of::<T>(),
        align_of::<T>()
    );
}
