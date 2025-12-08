use crate::typechecker::type_ast::Type;
use crate::vm::value::{NativeFn, Value};

pub struct NativeDef {
    pub name: &'static str,
    pub type_: Type,
    pub func: NativeFn,
}

pub fn get_natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "print",
            type_: Type::new_function(vec![Type::Any], Type::Void),
            func: |args| {
                for arg in args {
                    print!("{} ", arg);
                }
                println!();
                Value::Nil
            },
        },
        NativeDef {
            name: "assert",
            type_: Type::new_function(vec![Type::Any, Type::Any], Type::Void),
            func: |args| {
                if args[0] != args[1] {
                    panic!("Assertion failed: {} != {}", args[0], args[1]);
                }
                Value::Nil
            },
        },
        NativeDef {
            name: "clock",
            type_: Type::new_function(vec![], Type::Number),
            func: |_| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let start = SystemTime::now();
                let since_the_epoch = start.duration_since(UNIX_EPOCH).unwrap();
                Value::Number(since_the_epoch.as_secs_f64())
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
