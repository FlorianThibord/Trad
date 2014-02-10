(* Florian Thibord  --  Projet CERCLES *)

type ident = string

type value =
  Bool of bool
| Int of int
| Float of float

type base_type =
  T_Bool
| T_Int
| T_Float

type op_arith1 =
 Op_minus | Op_cast_real | Op_cast_int

type op_arith2 =
  Op_eq | Op_neq
| Op_add | Op_sub | Op_mul | Op_div | Op_mod
| Op_div_f

type op_logic =
  Op_and | Op_or | Op_xor

type op_relat =
| Op_lt | Op_le | Op_gt | Op_ge
