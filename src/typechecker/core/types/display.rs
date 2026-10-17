use crate::typechecker::core::types::Type;
use crate::typechecker::system::TypeSystem;

fn print_generics_with_sys(args: &[Type], sys: &TypeSystem) -> String {
    if args.is_empty() {
        String::new()
    } else {
        let formatted: Vec<String> = args.iter().map(|arg| arg.display_type(sys)).collect();
        format!("<{}>", formatted.join(", "))
    }
}

impl Type {
    pub fn display_type(&self, sys: &TypeSystem) -> String {
        match self {
            Type::Error => "Error".to_string(),
            Type::Infer(n) => format!("?{}", n),
            Type::Metatype(name, generic_args) => {
                format!(
                    "{}{}",
                    sys.get_name(*name),
                    print_generics_with_sys(generic_args, sys)
                )
            }
            Type::GenericParam(id) => sys.get_generic(*id).name.to_string(),
            Type::Number => "number".to_string(),
            Type::Boolean => "boolean".to_string(),
            Type::String => "string".to_string(),
            Type::Void => "void".to_string(),
            Type::Never => "never".to_string(),
            Type::Function(function_type) => {
                format!(
                    "func({}) -> {}",
                    function_type
                        .params
                        .iter()
                        .map(|t| t.1.display_type(sys))
                        .collect::<Vec<_>>()
                        .join(", "),
                    function_type.return_type.display_type(sys)
                )
            }
            Type::Unknown => "?".to_string(),
            Type::Any => "any".to_string(),
            Type::Struct(id, generic_args) => {
                format!(
                    "{}{}",
                    sys.get_struct(*id).name,
                    print_generics_with_sys(generic_args, sys)
                )
            }
            Type::Interface(id) => sys.get_interface(*id).name.to_string(),
            Type::Enum(id, generic_args) => {
                format!(
                    "{}{}",
                    sys.get_enum(*id).name,
                    print_generics_with_sys(generic_args, sys)
                )
            }
            Type::Optional(inner) => format!("{}?", inner.display_type(sys)),
            Type::Nil => "nil".to_string(),
            Type::Tuple(types) => {
                format!(
                    "({})",
                    types
                        .types
                        .iter()
                        .map(|t| t.display_type(sys))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::Span;
    use crate::typechecker::core::types::{FunctionType, TupleType};
    use std::rc::Rc;

    #[test]
    fn test_display_type_primitives() {
        let sys = TypeSystem::new();
        assert_eq!(Type::Number.display_type(&sys), "number");
        assert_eq!(Type::Boolean.display_type(&sys), "boolean");
        assert_eq!(Type::String.display_type(&sys), "string");
        assert_eq!(Type::Void.display_type(&sys), "void");
        assert_eq!(Type::Never.display_type(&sys), "never");
        assert_eq!(Type::Nil.display_type(&sys), "nil");
        assert_eq!(Type::Any.display_type(&sys), "any");
        assert_eq!(Type::Unknown.display_type(&sys), "?");
        assert_eq!(Type::Infer(1).display_type(&sys), "?1");
        assert_eq!(Type::Error.display_type(&sys), "Error");
    }

    #[test]
    fn test_display_type_composites() {
        let sys = TypeSystem::new();
        let opt = Type::Optional(Box::new(Type::Number));
        assert_eq!(opt.display_type(&sys), "number?");

        let tuple = Type::Tuple(Rc::new(TupleType {
            types: vec![Type::Number, Type::String, Type::Boolean],
        }));
        assert_eq!(tuple.display_type(&sys), "(number, string, boolean)");

        let func = Type::Function(Rc::new(FunctionType {
            is_vararg: false,
            params: vec![("a".into(), Type::Number), ("b".into(), Type::String)],
            return_type: Type::Boolean,
            type_params: vec![],
        }));
        assert_eq!(func.display_type(&sys), "func(number, string) -> boolean");
    }

    #[test]
    fn test_display_type_system_types() {
        let mut sys = TypeSystem::new();
        let point_id = sys.declare_struct(Span::default(), "Point".into(), &[], true);
        let point_ty = Type::Struct(point_id, Rc::from(vec![]));
        assert_eq!(point_ty.display_type(&sys), "Point");

        let list_id = sys.view_builtins().list_id;
        let list_ty = Type::Struct(list_id, Rc::from(vec![Type::Number]));
        assert_eq!(list_ty.display_type(&sys), "List<number>");

        let map_id = sys.view_builtins().map_id;
        let map_ty = Type::Struct(map_id, Rc::from(vec![Type::String, point_ty.clone()]));
        assert_eq!(map_ty.display_type(&sys), "Map<string, Point>");

        let color_id = sys.declare_enum(Span::default(), "Color".into(), &[], true);
        let color_ty = Type::Enum(color_id, Rc::from(vec![]));
        assert_eq!(color_ty.display_type(&sys), "Color");

        let iface_id = sys.declare_interface("Printable".into(), Span::default(), true);
        let iface_ty = Type::Interface(iface_id);
        assert_eq!(iface_ty.display_type(&sys), "Printable");

        let gen_id = sys.declare_generic("T".into(), Span::default());
        let gen_ty = Type::GenericParam(gen_id);
        assert_eq!(gen_ty.display_type(&sys), "T");

        let meta_ty = Type::Metatype(point_id.into(), Rc::from(vec![]));
        assert_eq!(meta_ty.display_type(&sys), "Point");
    }
}
