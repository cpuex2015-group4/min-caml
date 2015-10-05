open Asm

external gethi : float -> int32 = "gethi"
external getlo : float -> int32 = "getlo"

let stackset = ref S.empty (* ���Ǥ�Save���줿�ѿ��ν��� (caml2html: emit_stackset) *)
let stackmap = ref [] (* Save���줿�ѿ��Ρ������å��ˤ�������� (caml2html: emit_stackmap) *)
let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]
let savef x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    (let pad =
      if List.length !stackmap mod 2 = 0 then [] else [Id.gentmp Type.Int] in
    stackmap := !stackmap @ pad @ [x; x])
let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap
let offset x = 4 * List.hd (locate x)
let stacksize () = align (List.length !stackmap * 4)

let pp_id_or_imm = function
  | V(x) -> x
  | C(i) -> "$" ^ string_of_int i

(* �ؿ��ƤӽФ��Τ���˰������¤��ؤ���(register shuffling) (caml2html: emit_shuffle) *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let _, xys = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
  | [], [] -> []
  | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
      (y, sw) :: (x, y) :: shuffle sw (List.map
					 (function
					   | (y', z) when y = y' -> (sw, z)
					   | yz -> yz)
					 xys)
  | xys, acyc -> acyc @ shuffle sw xys

type dest = Tail | NonTail of Id.t (* �������ɤ�����ɽ���ǡ����� (caml2html: emit_dest) *)
let rec g oc = function (* ̿����Υ�����֥����� (caml2html: emit_g) *)
  | dest, Ans(exp) -> g' oc (dest, exp)
  | dest, Let((x, t), exp, e) ->
      g' oc (NonTail(x), exp);
      g oc (dest, e)
and g' oc = function (* ��̿��Υ�����֥����� (caml2html: emit_gprime) *)
  (* �����Ǥʤ��ä���׻���̤�dest�˥��å� (caml2html: emit_nontail) *)
  | NonTail(_), Nop -> Printf.fprintf oc "\tnop\n"
  | NonTail(x), Set(i) -> Printf.fprintf oc "\tli      %s, $%d\n" x i
  | NonTail(x), SetL(Id.L(y)) -> Printf.fprintf oc "\tlw      %s, $%s\n" x y
  | NonTail(x), Mov(y) ->
      if x <> y then Printf.fprintf oc "\tmove    %s, %s\n" x y
  | NonTail(x), Neg(y) ->
      Printf.fprintf oc "\tsub     %s, $zero, %s\n" x y
  | NonTail(x), Add(y, z') ->
      (match z' with
      | V(z) -> Printf.fprintf oc "\tadd     %s, %s, %s\n" x y z
      | C(i) -> Printf.fprintf oc "\taddi    %s, %s, $%d\n" x y i)
  | NonTail(x), Sub(y, z') ->
      (match z' with
      | V(z) -> Printf.fprintf oc "\tsub      %s, %s, %s\n" x y z
      | C(i) -> (
        Printf.fprintf oc "\tli      %s, $%d\n" x i;
        Printf.fprintf oc "\tsub     %s, %s, %s\n" x y x))
  | NonTail(x), Ld(y, V(z), i) -> Printf.fprintf oc "\tmovl\t(%s,%s,%d), %s\t# x86\n" y z i x
  | NonTail(x), Ld(y, C(j), i) -> Printf.fprintf oc "\tlw      %s, %d(%s)\n" x (j * i) y
  | NonTail(_), St(x, y, V(z), i) -> Printf.fprintf oc "\tmovl\t%s, (%s,%s,%d)\t# x86\n" x y z i
  | NonTail(_), St(x, y, C(j), i) -> Printf.fprintf oc "\tsw      %s, %d(%s)\n" x (j * i) y
  | NonTail(x), FMovD(y) ->
      if x <> y then Printf.fprintf oc "\tlw.s    %s, %s\n" x y
  | NonTail(x), FNegD(y) ->
      Printf.fprintf oc "\tsub.s   %s, $zero, %s\n" x y
  | NonTail(x), FAddD(y, z) ->
      Printf.fprintf oc "\tadd.s   %s, %s, %s\n" x y z
  | NonTail(x), FSubD(y, z) ->
      Printf.fprintf oc "\tsub.s   %s, %s, %s\n" x y z
  | NonTail(x), FMulD(y, z) ->
      Printf.fprintf oc "\tmult.s   %s, %s, %s\n" x y z
  | NonTail(x), FDivD(y, z) ->
      Printf.fprintf oc "\tdiv.s   %s, %s, %s\n" x y z
  | NonTail(x), LdDF(y, V(z), i) -> Printf.fprintf oc "\tmovsd\t(%s,%s,%d), %s\t# x86\n" y z i x
  | NonTail(x), LdDF(y, C(j), i) -> Printf.fprintf oc "\tlw.s    %s, %d(%s)\n" x (j * i) y
  | NonTail(_), StDF(x, y, V(z), i) -> Printf.fprintf oc "\tmovsd\t%s, (%s,%s,%d)\t# x86\n" x y z i
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.fprintf oc "\tsw.s    %s, %d(%s)\n" x (j * i) y
  | NonTail(_), Comment(s) -> Printf.fprintf oc "\t# %s\n" s
  (* ����β���̿��μ��� (caml2html: emit_save) *)
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
      save y;
      Printf.fprintf oc "\tsw      %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
      savef y;
      Printf.fprintf oc "\tsw.s    %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); ()
  (* �����β���̿��μ��� (caml2html: emit_restore) *)
  | NonTail(x), Restore(y) when List.mem x allregs ->
      Printf.fprintf oc "\tlw      %s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(x), Restore(y) ->
      assert (List.mem x allfregs);
      Printf.fprintf oc "\tlw.s    %s, %d(%s)\n" x (offset y) reg_sp
  (* �������ä���׻���̤����쥸�����˥��åȤ���ret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
      g' oc (NonTail(Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "\tjr\n";
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
      g' oc (NonTail(regs.(0)), exp);
      Printf.fprintf oc "\tjr\n";
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
      g' oc (NonTail(fregs.(0)), exp);
      Printf.fprintf oc "\tjr\n";
  | Tail, (Restore(x) as exp) ->
      (match locate x with
      | [i] -> g' oc (NonTail(regs.(0)), exp)
      | [i; j] when i + 1 = j -> g' oc (NonTail(fregs.(0)), exp)
      | _ -> assert false);
      Printf.fprintf oc "\tjr\n";
  | Tail, IfEq(x, V(y), e1, e2) ->
      g'_tail_if oc e1 e2 "beq"
      (Printf.sprintf "bne     %s, %s, " x y)
  | Tail, IfEq(x, C(i), e1, e2) ->
      Printf.fprintf oc "\tsub     %s, %s, $%d\n" x x i;
      g'_tail_if oc e1 e2 "beq"
      (Printf.sprintf "bne     %s, $zero, " x)
  | Tail, IfLE(x, y', e1, e2) ->
      let tmp = "$t8" in
      (match y' with
      | V(y) -> Printf.fprintf oc "\tslt     %s, %s, %s\n" tmp y x
      | C(i) -> Printf.fprintf oc "\tslti    %s, $%d, %s\n" tmp i x);
      g'_tail_if oc e1 e2 "beq"
      (Printf.sprintf "bne     %s, $zero, " tmp)
  | Tail, IfGE(x, y', e1, e2) ->
      let tmp = "$t8" in
      (match y' with
      | V(y) -> Printf.fprintf oc "\tslt     %s, %s, %s\n" tmp x y 
      | C(i) -> Printf.fprintf oc "\tslti    %s, %s, $%d\n" tmp x i);
      g'_tail_if oc e1 e2 "beq"
      (Printf.sprintf "beq     %s, $zero, " tmp)
  | Tail, IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.eq.s  %s, %s\n" x y;
      g'_tail_if oc e1 e2 "bclt" "bclf"
  | Tail, IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.le.s  %s, %s\n" x y;
      g'_tail_if oc e1 e2 "bclt" "bclf"
  | NonTail(z), IfEq(x, V(y), e1, e2) ->
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq"
      (Printf.sprintf "bne     %s, %s, " x y)
  | NonTail(z), IfEq(x, C(i), e1, e2) ->
      Printf.fprintf oc "\tsub     %s, %s, $%d\n" x x i;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq"
      (Printf.sprintf "bne     %s, $zero, " x)
  | NonTail(z), IfLE(x, y', e1, e2) ->
      let tmp = "$t8" in
      (match y' with
      | V(y) -> Printf.fprintf oc "\tslt     %s, %s, %s\n" tmp y x
      | C(i) -> Printf.fprintf oc "\tslti    %s, $%d, %s\n" tmp i x);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "beq"
      (Printf.sprintf "bne     %s, $zero, " tmp)
  | NonTail(z), IfGE(x, y', e1, e2) ->
      let tmp = "$t8" in
      (match y' with
      | V(y) -> Printf.fprintf oc "\tslt     %s, %s, %s\n" tmp x y 
      | C(i) -> Printf.fprintf oc "\tslti    %s, %s, $%d\n" tmp x i);
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bne"
      (Printf.sprintf "beq     %s, $zero, " tmp)
  | NonTail(z), IfFEq(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.eq.s  %s, %s\n" x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bclt" "bclf"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
      Printf.fprintf oc "\tc.le.s  %s, %s\n" x y;
      g'_non_tail_if oc (NonTail(z)) e1 e2 "bclt" "bclf"
  (* �ؿ��ƤӽФ��β���̿��μ��� (caml2html: emit_call) *)
  | Tail, CallCls(x, ys, zs) -> (* �����ƤӽФ� (caml2html: emit_tailcall) *)
      g'_args oc [(x, reg_cl)] ys zs;
      Printf.fprintf oc "\tj       *(%s)\n" reg_cl;
  | Tail, CallDir(Id.L(x), ys, zs) -> (* �����ƤӽФ� *)
      g'_args oc [] ys zs;
      Printf.fprintf oc "\tj       %s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
      g'_args oc [(x, reg_cl)] ys zs;
      let ss = stacksize () in
      if ss > 0 then Printf.fprintf oc "\tadd     %s, %s, $%d\n" reg_sp reg_sp ss;
      (* TODO: save $ra *)
      Printf.fprintf oc "\tjal     *(%s)\n" reg_cl;
      if ss > 0 then Printf.fprintf oc "\tsub     %s, %s, $%d\n" reg_sp reg_sp ss;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmove    %s, %s\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tmove.s  %s, %s\n" a fregs.(0)
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
      g'_args oc [] ys zs;
      let ss = stacksize () in
      if ss > 0 then Printf.fprintf oc "\tadd     %s, %s, $%d\n" reg_sp reg_sp ss;
      (* TODO: save $ra *)
      Printf.fprintf oc "\tjal     %s\n" x;
      if ss > 0 then Printf.fprintf oc "\tsub     %s, %s, $%d\n" reg_sp reg_sp ss;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "\tmove    %s, %s\n" a regs.(0)
      else if List.mem a allfregs && a <> fregs.(0) then
        Printf.fprintf oc "\tmove.s  %s, %s\n" a fregs.(0)
and g'_tail_if oc e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  Printf.fprintf oc "\t%s%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)
and g'_non_tail_if oc dest e1 e2 b bn =
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "\tj       %s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2
and g'_args oc x_reg_cl ys zs =
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "\tmove    %s, %s\n" y r)
    (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs in
  List.iter
    (fun (z, fr) -> Printf.fprintf oc "\tmove.s  %s, %s\n" z fr)
    (shuffle sw zfrs)

let h oc { name = Id.L(x); args = _; fargs = _; body = e; ret = _ } =
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog(data, fundefs, e)) =
  Format.eprintf "generating assembly...@.";
  Printf.fprintf oc ".data\n";
  List.iter
    (fun (Id.L(x), d) ->
      Printf.fprintf oc "%s:\t# %f\n" x d;
      Printf.fprintf oc "\t.long\t0x%lx\n" (gethi d);
      Printf.fprintf oc "\t.long\t0x%lx\n" (getlo d))
    data;
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail(regs.(0)), e);
  Printf.fprintf oc "\thlt\n";
