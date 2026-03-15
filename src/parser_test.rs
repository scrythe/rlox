use core::panic;
use std::marker::PhantomData;

use crate::scanner_new::{Scanner, Token, TokenType};

macro_rules! lt_to_PhantomData {
    ($temp:tt) => {
        PhantomData
    };
}

macro_rules! phantom_lt_data {
    ($lt:lifetime) => {
        PhantomData<&$lt ()>
    };
    ($($lt:lifetime),*) => {
        ( $(PhantomData<&$lt ()>),* )
    };
}

macro_rules! define_ast {
    (
        $enum_class:ident<$($enum_lts:lifetime),+>;
        $(
            $class_method_name:ident,
            $class_types:ident $(<$($lt:lifetime),*>)?
                -> $(
                    $field_names:ident: $field_class_types:ty $(= $field_class_res_types:ty)?
                ),+;
        )+
    ) => {
        pub enum $enum_class<$($enum_lts),*> {
            $(
                $class_types( $class_types $(<$($lt),*>)?)
            ),+
        }
        impl<$($enum_lts),+> $enum_class<$($enum_lts),+> {
            $(
                pub fn $class_method_name ($($field_names: $field_class_types),+)
                    -> Self {
                        $enum_class::$class_types(
                            $class_types {
                                $($field_names),*,
                                $(
                                    _markers: ( $(lt_to_PhantomData!($lt)),* )
                                )?
                            }
                        )
                    }
            )+
        }
        $(
            pub struct $class_types $(<$($lt),*>)? {
                $(
                    pub $field_names: $field_class_types,
                )+
                 $(
                    _markers: phantom_lt_data!($($lt),*),
                )?
            }
        )+
    };
}

define_ast!(
    Expr<'expr_lt, 'strings_lt>;
    assign_expr, Assign<'expr_lt, 'strings_lt> -> name: u32 = &'strings_lt str, value: u32 = Expr<'expr_lt>;
    binary_expr, Binary<'expr_lt> -> left: u32 = Expr<'expr_lt>, operator: TokenType, right: u32 = Expr<'expr_lt>;
);
