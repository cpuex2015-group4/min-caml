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
  | (Not(e),_) -> printf "NOT\n"; print_syntax' e (nest+1)
  | (Neg(e),_) -> printf "NEG\n"; print_syntax' e (nest+1)
  | (Mul(e1, e2),_) -> printf "MUL\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
  | (Div(e1, e2),_) -> printf "DIV\n"; print_syntax' e1 (nest+1); print_syntax' e2 (nest+1);
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
  | Mul(x, y) -> printf "MUL %s %s\n" x y
  | Div(x, y) -> printf "DIV %s %s\n" x y
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
  | ExtVar(x, t) -> printf "EXTVAR %s\n" x
  | ExtArray(x) -> printf "EXTARRAY %s\n" x
  | ExtFunApp(x, ys) -> (
    printf "EXTFUNAPP %s ( " x;
    List.iter (fun x -> printf "%s " x) ys;
    print_string ")\n")
in print_knormal' e 0

let print_closure (Closure.Prog(fds, t)) = 
  let rec print_closure' expr nest =
    print_indent nest;
    match expr with
    | Closure.Unit -> printf "UNIT\n"
    | Closure.Int(i) -> printf "INT %d\n" i
    | Closure.Float(f) -> printf "FLOAT %f\n" f
    | Closure.Neg(x) -> printf "NEG %s\n" x
    | Closure.Mul(x, y) -> printf "MUL %s %s\n" x y
    | Closure.Div(x, y) -> printf "DIV %s %s\n" x y
    | Closure.Add(x, y) -> printf "ADD %s %s\n" x y
    | Closure.Sub(x, y) -> printf "SUB %s %s\n" x y
    | Closure.FNeg(x) -> printf "FNEG %s\n" x
    | Closure.FAdd(x, y) -> printf "FADD %s %s\n" x y
    | Closure.FSub(x, y) -> printf "FSUB %s %s\n" x y
    | Closure.FMul(x, y) -> printf "FMUL %s %s\n" x y
    | Closure.FDiv(x, y) -> printf "FDIV %s %s\n" x y
    | Closure.IfEq(x, y, e1, e2) -> (
      printf "IFEQ %s %s\n" x y;
      print_closure' e1 (nest+1);
      print_closure' e2 (nest+1))
    | Closure.IfLE(x, y, e1, e2) -> (
      printf "IFLE %s %s\n" x y;
      print_closure' e1 (nest+1);
      print_closure' e2 (nest+1))
    | Closure.Let((x, t), e1, e2) -> (
        printf "LET %s\n" x;
      print_closure' e1 (nest+1);
      print_closure' e2 (nest+1))
    | Closure.Var(x) -> printf "VAR %s\n" x
    | Closure.MakeCls((x, t), {
      Closure.entry = Id.L(xc);
      Closure.actual_fv = ycs }, tr) -> (
        printf "MAKECLS %s : %s ( " x xc;
        List.iter (fun y -> printf "%s " y) ycs;
        printf ")\n")
    | Closure.AppCls(x, ys) -> (
      printf "APPCLS %s ( " x;
      List.iter (fun y -> printf "%s " y) ys;
      print_string ")\n")
    | Closure.AppDir(Id.L(x), ys) -> (
      printf "APPDIR %s ( " x;
      List.iter (fun y -> printf "%s " y) ys;
      print_string ")\n")
    | Closure.Tuple(xs) -> 
        print_string "TUPLE ( ";
      List.iter (fun x -> printf "%s " x) xs;
      print_string ")"
    | Closure.LetTuple(xts, y, e) -> (
      print_string "LETTUPLE ( ";
      List.iter (fun p -> let (id, t) = p in printf "%s " id) xts;
      printf ") %s" y;
      print_closure' e (nest+1))
    | Closure.Get(x, y) -> printf "GET %s %s\n" x y
    | Closure.Put(x, y, z) -> printf "PUT %s %s %s\n" x y z
    | Closure.ExtVar(Id.L(x), t) -> printf "EXTVAR %s\n" x
    | Closure.ExtArray(Id.L(x)) -> printf "EXTARRAY %s\n" x
  in
  let print_fundef
  { Closure.name = (Id.L(x), t);
    Closure.args = ys;
    Closure.formal_fv = zs;
    Closure.body = e } =
    printf "#### FUNDEF %s (" x;
    List.iter (fun (y,t) -> printf " %s" y) ys;
    print_string " )\n";
    print_closure' e 0
  in List.iter (fun fd -> print_fundef fd) fds

let print_asmprog prog =
  let print_data d =
    List.iter (fun (Id.L(id), v) -> printf "\t%10s -> %f\n" id v) d in
  let rec print_exp = function
    | Asm.Nop -> printf "\tNOP\n"
    | Asm.Set(i) -> printf "\tSET %d\n" i
    | Asm.SetL(Id.L(x)) -> printf "\tSETL %s\n" x
    | Asm.Mov(x) -> printf "\tMOV %s\n" x
    | Asm.Neg(x) -> printf "\tNEG %s\n" x
    | Asm.Mul(x, V(y)) -> printf "\tMUL %s, %s\n" x y
    | Asm.Mul(x, C(i)) -> printf "\tMULI %s, %d\n" x i
    | Asm.Div(x, V(y)) -> printf "\tDIV %s, %s\n" x y
    | Asm.Div(x, C(i)) -> printf "\tDIVI %s, %d\n" x i
    | Asm.Add(x, V(y)) -> printf "\tADD %s, %s\n" x y
    | Asm.Add(x, C(i)) -> printf "\tADDI %s, %d\n" x i
    | Asm.Sub(x, V(y)) -> printf "\tSUB %s, %s\n" x y
    | Asm.Sub(x, C(i)) -> printf "\tSUBI %s, %d\n" x i
    | Asm.Ld(x, V(y)) -> printf "\tLD (%s,%s)\n" x y
    | Asm.Ld(x, C(i)) -> printf "\tLD %d(%s)\n" i x
    | Asm.St(x, y, V(z)) -> printf "\tST %s, (%s,%s)\n" x y z
    | Asm.St(x, y, C(i)) -> printf "\tST %s, %d(%s)\n" x i y
    | Asm.FMovD(x) -> printf "\tFMOV %s\n" x
    | Asm.FNegD(x) -> printf "\tFNEG %s\n" x
    | Asm.FAddD(x, y) -> printf "\tFADD %s, %s\n" x y
    | Asm.FSubD(x, y) -> printf "\tFSUB %s, %s\n" x y
    | Asm.FMulD(x, y) -> printf "\tFMUL %s, %s\n" x y
    | Asm.FDivD(x, y) -> printf "\tFDIV %s, %s\n" x y
    | Asm.LdDF(x, V(y)) -> printf "\tLDDF (%s,%s)\n" x y
    | Asm.LdDF(x, C(i)) -> printf "\tLDDF %d(%s)\n" i x
    | Asm.StDF(x, y, V(z)) -> printf "\tSTDF %s, (%s,%s)\n" x y z
    | Asm.StDF(x, y, C(i)) -> printf "\tSTDF %s, %d(%s)\n" x i y
    | Asm.Comment(s) -> printf "\t# %s\n" s
    (* virtual instructions *)
    | Asm.IfEq(x, V(y), e1, e2) -> printf "\tIFEQ %s, %s\n" x y; print_cmd e1; print_cmd e2
    | Asm.IfEq(x, C(i), e1, e2) -> printf "\tIFEQ %s, %d\n" x i; print_cmd e1; print_cmd e2
    | Asm.IfLE(x, V(y), e1, e2) -> printf "\tIFLE %s, %s\n" x y; print_cmd e1; print_cmd e2
    | Asm.IfLE(x, C(i), e1, e2) -> printf "\tIFLE %s, %d\n" x i; print_cmd e1; print_cmd e2
    | Asm.IfGE(x, V(y), e1, e2) -> printf "\tIFGE %s, %s\n" x y; print_cmd e1; print_cmd e2
    | Asm.IfGE(x, C(i), e1, e2) -> printf "\tIFGE %s, %d\n" x i; print_cmd e1; print_cmd e2
    | Asm.IfFEq(x, y, e1, e2) -> printf "\tIFFEQ %s, %s\n" x y; print_cmd e1; print_cmd e2
    | Asm.IfFLE(x, y, e1, e2) -> printf "\tIFFLE %s, %s\n" x y; print_cmd e1; print_cmd e2
    (* closure address, integer arguments, and float arguments *)
    | Asm.CallCls(x, ys, zs) -> printf "\tCALLCLS %s\n" x
    | Asm.CallDir(Id.L(x), ys, zs) -> printf "\tCALLDIR %s\n" x
    | Asm.Save(x, y) -> printf "\tSAVE %s, %s\n" x y
    | Asm.Restore(x) -> printf "\tRESTORE %s\n" x
  and print_cmd = function
    | Asm.Ans(e) -> print_exp e
    | Asm.Let((x, t), e, cmd) -> printf "\tLET %s\n" x; print_exp e; print_cmd cmd in
  let print_fundef { name = Id.L(x); args = ys; fargs = zs; body = e; ret = t } =
    printf "\tFUNDEF %s (" x;
    List.iter (fun y -> printf " %s" y) ys;
    print_string " )\n";
    print_cmd e in
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

let cse = knormal

let closure e = print_closure e
