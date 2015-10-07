type t = (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let rec of_string tp =
  match tp with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun([], ty) -> of_string ty
  | Fun(tx::txs, ty) ->
      (of_string tx) ^ " -> " ^ (of_string (Fun(txs,ty)))
  | Tuple([]) -> ")"
  | Tuple(tx::txs) ->
      "(" ^ (of_string tx) ^ ", "
  | Array t -> (of_string t) ^ " array"
  | Var _ -> "var"
