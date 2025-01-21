(* DO NOT change the definition of this type *)
type cond =
    Equal of int * int    (* True if 'i1' is equal to 'i2' *)
  | NotEqual of int * int (* True if 'i1' not equal to 'i2' *)
  | LessThan of int * int (* True if 'i1' is less than 'i2' *)
  | And of cond * cond
  | Or of cond * cond
  | Not of cond

(* Return the boolean value represented by the condition 'c'. *)
let rec eval c =
  match c with
  | Equal (i1, i2) -> i1 = i2
  | NotEqual (i1, i2) -> i1 <> i2 (* TODO *)
  | LessThan (i1, i2) -> i1 < i2 (* TODO *)
  | And (c1, c2) -> (eval c1) && (eval c2) (* TODO *)
  | Or (c1, c2) -> (eval c1) || (eval c2) (* TODO *)
  | Not c1 -> not (eval c1) (* TODO *)
