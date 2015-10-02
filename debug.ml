open Printf
open Syntax

let print_syntax e =
let rec print_syntax' expr nest =
  printf "%*s" (nest * 2) "";
  match expr with
  | Not(e) -> printf "NOT\n"; print_syntax' e (nest-1)
  | Neg(e) -> printf "NEG\n"; print_syntax' e (nest-1)
  | Add(e1, e2) -> printf "ADD\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | Sub(e1, e2) -> printf "SUB\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | Eq(e1, e2) -> printf "EQ\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | LE(e1, e2) -> printf "LE\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | FNeg(e) -> printf "FNEG\n"; print_syntax' e (nest-1)
  | FAdd(e1, e2) -> printf "FADD\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | FSub(e1, e2) -> printf "FSUB\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | FMul(e1, e2) -> printf "FMUL\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | FDiv(e1, e2) -> printf "FDIV\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | If(e1, e2, e3) -> (
    printf "IF\n"; print_syntax' e1 (nest-1);
    print_syntax' e2 (nest-1); print_syntax' e3 (nest-1))
  | Let(xt, e1, e2) -> (
    let id, t = xt in
    printf "LET %s\n" id;
    print_syntax' e1 (nest-1);
    print_syntax' e2 (nest-1))
  | LetRec({ name = xt; args = yts; body = e1}, e2) -> (
      let id, t = xt in
      printf "LETREC %s( " id;
      List.map (fun p -> let (id, t) = p in printf "%s " id) yts;
      print_string ")\n";
      print_syntax' e1 (nest-1);
      print_syntax' e2 (nest-1))
  | App(e, es) -> (
    match e with
    | Var(f) -> printf "APP %s\n" f
    | _ -> print_syntax' e (nest-1)
  ); List.map (fun e -> print_syntax' e (nest-1)) es; ()
  | Tuple(es) -> printf "TUPLE\n"; List.map (fun e -> print_syntax' e (nest-1)) es; ()
  | LetTuple(xts, e1, e2) -> (
    printf "LETTUPLE ( ";
    List.map (fun p -> let (id, t) = p in printf "%s " id) xts;
    print_string ")\n";
    print_syntax' e1 (nest-1); print_syntax' e2 (nest-1))
  | Array(e1, e2) -> printf "ARRAY\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | Get(e1, e2) -> printf "GET\n"; print_syntax' e1 (nest-1); print_syntax' e2 (nest-1);
  | Put(e1, e2, e3) -> (
    printf "PUT\n"; print_syntax' e1 (nest-1);
    print_syntax' e2 (nest-1); print_syntax' e3 (nest-1))
  | Unit -> printf "()\n"
  | Bool(b) -> if b then printf "BOOL #t\n" else printf "BOOL #f\n"
  | Int(i) -> printf "INT %d\n" i
  | Float(f) -> printf "FLOAT %f\n" f
  | Var(v) -> printf "VAR %s\n" v
in print_syntax' e 0

let parse e =
  print_syntax e

let knormal e =
  printf "hoge"
