use crate::typechecker::core::ast::{BinaryOp, ExprKind, LogicalOp, TypedExpr, UnaryOp};
use crate::typechecker::core::types::Type;
use crate::typechecker::{Symbol, TypeChecker};

type Refinement = (Symbol, Type);

#[derive(Debug)]
pub(crate) struct BranchRefinements {
    pub(crate) true_path: Vec<Refinement>,
    pub(crate) false_path: Vec<Refinement>,
}

impl<'src> TypeChecker<'src> {
    pub(crate) fn analyze_condition(&mut self, expr: &TypedExpr) -> BranchRefinements {
        match &expr.kind {
            // x == nil
            ExprKind::Is {
                target,
                variant_idx,
            } => {
                if let ExprKind::GetVar(_, name) = &target.kind
                    && let Type::Enum(enum_name, generics) = &target.ty
                {
                    let enum_def = self.sys.get_enum(enum_name).unwrap();
                    let false_path = if enum_def.variants.len() == 2 {
                        let other_ty = enum_def
                            .get_variant_by_index((1 - *variant_idx) as usize, generics)
                            .unwrap();
                        vec![(name.clone(), other_ty)]
                    } else {
                        vec![]
                    };
                    let narrowed_type = enum_def
                        .get_variant_by_index(*variant_idx as usize, generics)
                        .unwrap();
                    return BranchRefinements {
                        true_path: vec![(name.clone(), narrowed_type)],
                        false_path,
                    };
                }
                BranchRefinements {
                    true_path: vec![],
                    false_path: vec![],
                }
            }
            ExprKind::Binary {
                left,
                operator,
                right,
            } if operator == &BinaryOp::EqualEqual => {
                let other = if right.ty == Type::Nil {
                    left
                } else if left.ty == Type::Nil {
                    right
                } else {
                    return BranchRefinements {
                        true_path: vec![],
                        false_path: vec![],
                    };
                };

                if let ExprKind::GetVar(_, name) = &other.kind
                    && let Type::Optional(inner) = &other.ty
                {
                    return BranchRefinements {
                        true_path: vec![],
                        false_path: vec![(name.clone(), *inner.clone())],
                    };
                }
                BranchRefinements {
                    true_path: vec![],
                    false_path: vec![],
                }
            }
            // Case: !Condition
            ExprKind::Unary { operator, operand } if operator == &UnaryOp::Not => {
                let inner = self.analyze_condition(operand);
                // Swap the branches
                BranchRefinements {
                    true_path: inner.false_path,
                    false_path: inner.true_path,
                }
            }
            ExprKind::Logical {
                left,
                operator,
                right,
                ..
            } if operator == &LogicalOp::And => {
                let refine_left = self.analyze_condition(left);
                let refine_right = self.analyze_condition(right);
                BranchRefinements {
                    true_path: [refine_left.true_path, refine_right.true_path].concat(),
                    false_path: vec![],
                }
            }
            _ => BranchRefinements {
                true_path: vec![],
                false_path: vec![],
            },
        }
    }
}
