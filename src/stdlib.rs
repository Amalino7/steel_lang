use crate::typechecker::types::Type;
use crate::vm::value::{NativeFn, Value};

pub struct NativeDef {
    pub name: &'static str,
    pub type_: Type,
    pub func: NativeFn,
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
                    .Ok(val) => {return Result.<NewOk, Err>.Ok(f(val));}
                    .Err(err) => {return Result.<NewOk, Err>.Err(err);}
                }
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
            func: |args, _| {
                return Err(args[0].to_string());
            },
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
                return Ok(Value::Nil);
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
