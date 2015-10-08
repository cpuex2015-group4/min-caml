open Printf
open Syntax
open KNormal

let rec print_indent n =
  if n > 0 then (print_string "| "; print_indent (n-1)) else ()

let print_syntax e =
let rec print_syntax' expr nest =
  print_indent nest;
  match expr with
  | (Not(e),_) -> printf "NOT\n"; print_syntax' e (nest+1)
  | (Neg(e),_) -> printf "NEG\n"; print_syntax' e (nest+1)
  | (Add(e1, e2),_) -> printf "ADD\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Sub(e1, e2),_) -> printf "SUB\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Eq(e1, e2),_) -> printf "EQ\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (LE(e1, e2),_) -> printf "LE\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FNeg(e),_) -> printf "FNEG\n"; print_syntax' e (nest+1)
  | (FAdd(e1, e2),_) -> printf "FADD\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FSub(e1, e2),_) -> printf "FSUB\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FMul(e1, e2),_) -> printf "FMUL\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FDiv(e1, e2),_) -> printf "FDIV\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (If(e1, e2, e3),_) -> (
    printf "IF\n"; print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1); print_syntax' e3 (nest+1))
  | (Let(xt, e1, e2),_) -> (
    let id, t = xt in
    printf "LET %s\n" id;
    print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1))
  | (LetRec({ name = xt; args = yts; body = e1}, e2),_) -> (
      let id, t = xt in
      printf "LETREC %s( " id;
      List.iter (fun p -> let (id, t) = p in printf "%s " id) yts;
      print_string ")\n";
      print_syntax' e1 (nest+1);
      print_syntax' e2 (nest+1))
  | (App(e, es),_) -> (
    match e with
    | (Var(f),_) -> printf "APP %s\n" f
    | _ -> print_syntax' e (nest+1)
  ); List.iter (fun e -> print_syntax' e (nest+1)) es
  | (Tuple(es),_) -> printf "TUPLE\n"; List.iter (fun e -> print_syntax' e (nest+1)) es
  | (LetTuple(xts, e1, e2),_) -> (
    printf "LETTUPLE ( ";
    List.iter (fun p -> let (id, t) = p in printf "%s " id) xts;
    print_string ")\n";
    print_syntax' e1 (nest+1); print_syntax' e2 (nest+1))
  | (Array(e1, e2),_) -> printf "ARRAY\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Get(e1, e2),_) -> printf "GET\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Put(e1, e2, e3),_) -> (
    printf "PUT\n"; print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1); print_syntax' e3 (nest+1))
  | (Unit,_) -> printf "()\n"
  | (Bool(b),_) -> if b then printf "BOOL #t\n" else printf "BOOL #f\n"
  | (Int(i),_) -> printf "INT %d\n" i
  | (Float(f),_) -> printf "FLOAT %f\n" f
  | (Var(v),_) -> printf "VAR %s\n" v
in print_syntax' e 0

let print_knormal e =
let rec print_knormal' expr nest =
  print_indent nest;
  match expr with
  | Unit -> printf "()\n"
  | Int(i) -> printf "INT %d\n" i
  | Float(d) -> printf "FLOAT %f\n" d
  | Neg(x) -> printf "NEG %s\n" x
  | Add(x, y) -> printf "ADD %s %s\n" x y
  | Sub(x, y) -> printf "SUB %s %s\n" x y
  | FNeg(x) -> printf "FNEG %s\n" x
  | FAdd(x, y) -> printf "FADD %s %s\n" x y
  | FSub(x, y) -> printf "FSUB %s %s\n" x y
  | FMul(x, y) -> printf "FMUL %s %s\n" x y
  | FDiv(x, y) -> printf "FDIV %s %s\n" x y
  | IfEq(x, y, e1, e2) -> (
    printf "IFEQ %s %s\n" x y;
    print_knormal' e1 (nest+1);
    print_knormal' e2 (nest+1))
  | IfLE(x, y, e1, e2) -> (
    printf "IFLE %s %s\n" x y;
    print_knormal' e1 (nest+1);
    print_knormal' e2 (nest+1))
  | Let((x, t), e1, e2) -> (
      printf "LET %s\n" x;
    print_knormal' e1 (nest+1);
    print_knormal' e2 (nest+1))
  | Var(x) -> printf "VAR %s\n" x
  | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (
    printf "LETREC %s( " x;
    List.iter (fun p -> let (id, t) = p in printf "%s " id) yts;
    print_string ")\n";
    print_knormal' e1 (nest+1);
    print_knormal' e2 (nest+1))
  | App(x, ys) -> (
    printf "APP %s ( " x;
    List.iter (fun y -> printf "%s " y) ys;
    print_string ")\n")
  | Tuple(xs) -> 
      print_string "TUPLE ( ";
    List.iter (fun x -> printf "%s " x) xs;
    print_string ")"
  | LetTuple(xts, y, e) -> (
    print_string "LETTUPLE ( ";
    List.iter (fun p -> let (id, t) = p in printf "%s " id) xts;
    printf ") %s" y;
    print_knormal' e (nest+1))
  | Get(x, y) -> printf "GET %s %s\n" x y
  | Put(x, y, z) -> printf "GET %s %s %s\n" x y z
  | ExtArray(x) -> printf "EXTARRAY %s\n" x
  | ExtFunApp(x, ys) -> (
    printf "EXTFUNAPP %s ( " x;
    List.iter (fun x -> printf "%s " x) ys;
    print_string ")\n")
in print_knormal' e 0

let parse e =
  print_syntax e

let knormal e =
  print_knormal e

let cse = knormal
