open Parsing
open Exception

type t_ = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
(* トークン位置情報付きの構文データ *)
(* syntax, トークン, 開始位置, 終了位置 *)
and t = t_ * (string * Lexing.position * Lexing.position)

(* Syntax.t -> Syntax.t_ (トークン位置情報を取り除く) *)
let syntax x =
  let (s,_) = x in s
(* Syntax.t_ -> Syntax.t (トークン位置情報を付与する) *)
let info x = 
  let start_p = Parsing.symbol_start_pos () in
  let end_p = Parsing.symbol_end_pos () in
  try
    let token = Bytes.sub_string !Exception.buffer
      (Parsing.symbol_start ())
      ((Parsing.symbol_end ()) - (Parsing.symbol_start ()))
    in (x, (token, start_p, end_p))
  with Invalid_argument _ -> (x, ("_", start_p, end_p))

