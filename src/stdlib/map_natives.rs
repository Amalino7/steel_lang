use crate::stdlib::NativeDef;
use crate::vm::value::{HashableValue, Map, Value};

pub(super) fn natives() -> Vec<NativeDef> {
    vec![
        NativeDef {
            name: "Map.len",
            type_: None,
            func: |args, _| {
                let map = as_map(&args[0]);
                Ok(Value::Number(map.map.len() as f64))
            },
        },
        NativeDef {
            name: "Map.contains_key",
            type_: None,
            func: |args, _| {
                let map = as_map(&args[0]);
                let key = &args[1];
                if let Value::Number(f) = key
                    && f.is_nan()
                {
                    return Err("NaN cannot be used as a map key".to_string());
                }
                Ok(Value::Boolean(map.map.contains_key(&HashableValue(*key))))
            },
        },
        NativeDef {
            name: "Map.remove",
            type_: None,
            func: |args, _| match args[0] {
                Value::Map(mut map) => unsafe {
                    let key = &args[1];
                    if let Value::Number(f) = key
                        && f.is_nan()
                    {
                        return Err("NaN cannot be used as a map key".to_string());
                    }
                    let removed = map.deref_mut().map.remove(&HashableValue(*key));
                    Ok(removed.unwrap_or(Value::Nil))
                },
                _ => unreachable!("expected map"),
            },
        },
    ]
}

fn as_map(val: &Value) -> &Map {
    match val {
        Value::Map(map) => map,
        _ => unreachable!("expected map"),
    }
}
