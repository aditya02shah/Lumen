(* --EXCEPTIONS-- *)
exception Internal_error (* should have been handled by the checker *)
exception Negative_power (* negative exponents *)

(* --INTERPRETER IMPLEMENTATION-- *)
let rec pow (base : int) (exp : int) : int =
  (* simple power function *)
  if exp < 0 then raise Negative_power
  else if exp = 0 then 1
  else base * pow base (exp - 1)

let rec eval_cond_expr (cond_expr : Ast.cond_expr) : bool =
  (* evaluate conditional expression *)
  match cond_expr with
  | BOOL_VAL (bv, _) -> (
      match bv with
      | BOOL b -> b
      | VAL_CMP (op, val1, val2) -> (
          (* compare values *)
          match op with
          | LESS_THAN -> val1 < val2
          | LESS_THAN_EQUALS -> val1 <= val2
          | GREATER_THAN -> val1 > val2
          | GREATER_THAN_EQUALS -> val1 >= val2
          | EQUALS -> val1 = val2
          | NOT_EQUALS -> val1 <> val2))
  | COMBINATION ((op, ce1, ce2), _) -> (
      (* apply combination *)
      match (op, eval_cond_expr ce1, eval_cond_expr ce2) with
      | LOGICAL_AND, true, true -> true
      | LOGICAL_AND, _, _ -> false
      | LOGICAL_OR, false, false -> false
      | LOGICAL_OR, _, _ -> true)

let rec eval_expr ((expr, pos) : Ast.expr) : Semantic_check.eval_value =
  (* evaluate expression *)
  match expr with
  | INT n -> INT n
  | VAR x -> (
      (* find var entry in env *)
      match (Hashtbl.find Semantic_check.var_env x).assigned_val with
      | EXPR e -> eval_expr e (* evaluate expr assigned to var & increment it *)
      | VAL v -> v (* var value has already been evaluated *)
      | NONE -> raise Internal_error)
  | INCREMENT x -> (
      (* find var entry in env *)
      match (Hashtbl.find Semantic_check.var_env x).assigned_val with
      | EXPR e -> (
          (* evaluate expr assigned to var & increment it *)
          match eval_expr e with
          | INT i -> INT (i + 1)
          | BOOL _ -> raise Internal_error)
      | VAL v -> (
          (* var value has already been evaluated; increment it *)
          match v with
          | INT i -> INT (i + 1)
          | BOOL _ -> raise Internal_error)
      | NONE -> raise Internal_error)
  | FUNC_CALL f -> INT 0 (* TODO: eval func_call *)
  | BINARY (op, exp1, exp2) -> (
      (* evaluate arithmetic expression *)
      match (eval_expr exp1, eval_expr exp2) with
      | INT e1, INT e2 -> (
          match op with
          | ADD -> INT (e1 + e2)
          | SUBTRACT -> INT (e1 - e2)
          | MULTIPLY -> INT (e1 * e2)
          | DIVIDE -> INT (e1 / e2)
          | MOD -> INT (e1 mod e2)
          | POW -> INT (pow e1 e2))
      | _, _ -> raise Internal_error)
  | COND ce -> BOOL (eval_cond_expr ce)
