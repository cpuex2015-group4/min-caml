open Printf
open Syntax
open KNormal
open Asm

let rec print_indent n =
  if n > 0 then (print_string "| "; print_indent (n-1)) else ()

let print_syntax e =
let rec print_syntax' expr nest =
  print_indent nest;
  match expr with
  | (Not(e),_,_,_,_) -> printf "NOT\n"; print_syntax' e (nest+1)
  | (Neg(e),_,_,_,_) -> printf "NEG\n"; print_syntax' e (nest+1)
  | (Add(e1, e2),_,_,_,_) -> printf "ADD\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Sub(e1, e2),_,_,_,_) -> printf "SUB\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Eq(e1, e2),_,_,_,_) -> printf "EQ\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (LE(e1, e2),_,_,_,_) -> printf "LE\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FNeg(e),_,_,_,_) -> printf "FNEG\n"; print_syntax' e (nest+1)
  | (FAdd(e1, e2),_,_,_,_) -> printf "FADD\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FSub(e1, e2),_,_,_,_) -> printf "FSUB\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FMul(e1, e2),_,_,_,_) -> printf "FMUL\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (FDiv(e1, e2),_,_,_,_) -> printf "FDIV\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (If(e1, e2, e3),_,_,_,_) -> (
    printf "IF\n"; print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1); print_syntax' e3 (nest+1))
  | (Let(xt, e1, e2),_,_,_,_) -> (
    let id, t = xt in
    printf "LET %s\n" id;
    print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1))
  | (LetRec({ name = xt; args = yts; body = e1}, e2),_,_,_,_) -> (
      let id, t = xt in
      printf "LETREC %s( " id;
      List.iter (fun p -> let (id, t) = p in printf "%s " id) yts;
      print_string ")\n";
      print_syntax' e1 (nest+1);
      print_syntax' e2 (nest+1))
  | (App(e, es),_,_,_,_) -> (
    match e with
    | (Var(f),_,_,_,_) -> printf "APP %s\n" f
    | _ -> print_syntax' e (nest+1)
  ); List.iter (fun e -> print_syntax' e (nest+1)) es
  | (Tuple(es),_,_,_,_) -> printf "TUPLE\n"; List.iter (fun e -> print_syntax' e (nest+1)) es
  | (LetTuple(xts, e1, e2),_,_,_,_) -> (
    printf "LETTUPLE ( ";
    List.iter (fun p -> let (id, t) = p in printf "%s " id) xts;
    print_string ")\n";
    print_syntax' e1 (nest+1); print_syntax' e2 (nest+1))
  | (Array(e1, e2),_,_,_,_) -> printf "ARRAY\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Get(e1, e2),_,_,_,_) -> printf "GET\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Put(e1, e2, e3),_,_,_,_) -> (
    printf "PUT\n"; print_syntax' e1 (nest+1);
    print_syntax' e2 (nest+1); print_syntax' e3 (nest+1))
  | (Unit,_,_,_,_) -> printf "()\n"
  | (Bool(b),_,_,_,_) -> if b then printf "BOOL #t\n" else printf "BOOL #f\n"
  | (Int(i),_,_,_,_) -> printf "INT %d\n" i
  | (Float(f),_,_,_,_) -> printf "FLOAT %f\n" f
  | (Var(v),_,_,_,_) -> printf "VAR %s\n" v
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

let print_asmprog prog =
  let print_data d =
    List.iter (fun (Id.L(id), v) -> printf "\t%10s -> %f\n" id v) d in
  let rec print_exp = function
    | Asm.Nop -> printf "\tNOP\n"
    | Asm.Set(i) -> printf "\tSET\n"
    | Asm.SetL(x) -> printf "\tSETL\n"
    | Asm.Mov(x) -> printf "\tMOV\n"
    | Asm.Neg(x) -> printf "\tNEG\n"
    | Asm.Add(x, v) -> printf "\tADD\n"
    | Asm.Sub(x, v) -> printf "\tSUB\n"
    | Asm.Ld(x, v) -> printf "\tLD\n"
    | Asm.St(x, y, v) -> printf "\tST\n"
    | Asm.FMovD(x) -> printf "\tFMOV\n"
    | Asm.FNegD(x) -> printf "\tFNEG\n"
    | Asm.FAddD(x, y) -> printf "\tFADD\n"
    | Asm.FSubD(x, y) -> printf "\tFSUB\n"
    | Asm.FMulD(x, y) -> printf "\tFMUL\n"
    | Asm.FDivD(x, y) -> printf "\tFDIV\n"
    | Asm.LdDF(x, v) -> printf "\tLDDF\n"
    | Asm.StDF(x, y, v) -> printf "\tSTDF\n"
    | Asm.Comment(s) -> printf "\t# %s\n" s
    (* virtual instructions *)
    | Asm.IfEq(x, v, e1, e2) -> printf "\tIFEQ\n"; print_cmd e1; print_cmd e2
    | Asm.IfLE(x, v, e1, e2) -> printf "\tIFLE\n"; print_cmd e1; print_cmd e2
    | Asm.IfGE(x, v, e1, e2) -> printf "\tIFGE\n"; print_cmd e1; print_cmd e2
    | Asm.IfFEq(x, v, e1, e2) -> printf "\tIFFEQ\n"; print_cmd e1; print_cmd e2
    | Asm.IfFLE(x, v, e1, e2) -> printf "\tIFFLE\n"; print_cmd e1; print_cmd e2
    (* closure address, integer arguments, and float arguments *)
    | Asm.CallCls(x, ys, zs) -> printf "\tCALLCLS\n"
    | Asm.CallDir(x, ys, zs) -> printf "\tCALLDIR\n"
    | Asm.Save(x, y) -> printf "\tSAVE\n"
    | Asm.Restore(x) -> printf "\tRESTORE\n"
  and print_cmd = function
    | Asm.Ans(e) -> print_exp e
    | Asm.Let((x, t), e, cmd) -> printf "\tLET %s\n" x; print_exp e; print_cmd cmd in
  let print_fundef { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } =
    printf "\tFUNDEF %s\n" x; print_cmd e in
  let Asm.Prog(data, fs, cmd) = prog in
  printf "[Data]\n";
  print_data data;
  printf "[FunDef]\n";
  List.iter (fun fd -> print_fundef fd) fs;
  printf "[Program]\n";
  print_cmd cmd

let parse e =
  print_syntax e

let knormal e =
  print_knormal e
