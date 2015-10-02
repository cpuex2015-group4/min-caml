open Printf
open Syntax

let rec deref_term = function
  | Not(e) -> printf "Not\n"
  | Neg(e) -> printf "Neg\n"
  | Add(e1, e2) -> printf "Add\n"
  | Sub(e1, e2) -> printf "Sub\n"
  | Eq(e1, e2) -> printf "Eq\n"
  | LE(e1, e2) -> printf "Le\n"
  | FNeg(e) -> printf "FNeg\n"
  | FAdd(e1, e2) -> printf "FAdd\n"
  | FSub(e1, e2) -> printf "FSub\n"
  | FMul(e1, e2) -> printf "FMul\n"
  | FDiv(e1, e2) -> printf "FDiv\n"
  | If(e1, e2, e3) -> printf "If\n"
  | Let(xt, e1, e2) -> printf "Let\n"
  | LetRec({ name = xt; args = yts; body = e1}, e2) -> (
      let id, t = xt in
      printf "LetRec %s\n" id;
      deref_term e2)
  | App(e, es) -> printf "App\n"
  | Tuple(es) -> printf "Tuple\n"
  | LetTuple(xts, e1, e2) -> printf "LetTuple\n"
  | Array(e1, e2) -> printf "Array\n"
  | Get(e1, e2) -> printf "Get\n"
  | Put(e1, e2, e3) -> printf "Put\n"
  | e -> printf "?\n"

let parse e =
  deref_term e

let knormal e =
  print_string "debug kNormalization"
