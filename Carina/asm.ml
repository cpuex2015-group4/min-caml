type id_or_imm = V of Id.t | C of int
type t = (* 命令の列 (caml2html: sparcasm_t) *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
and exp = (* 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) *)
  | Nop
  | Set of int
  | SetL of Id.l
  | Mov of Id.t
  | Neg of Id.t
  | Mul of Id.t * id_or_imm
  | Div of Id.t * id_or_imm
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm
  | St of Id.t * Id.t * id_or_imm
  | FMovD of Id.t
  | FNegD of Id.t
  | FAddD of Id.t * Id.t
  | FSubD of Id.t * Id.t
  | FMulD of Id.t * Id.t
  | FDivD of Id.t * Id.t
  | LdDF of Id.t * id_or_imm
  | StDF of Id.t * Id.t * id_or_imm
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save) *)
  | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)
type fundef = { name : Id.l; args : Id.t list; fargs : Id.t list; body : t; ret : Type.t }
(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog) *)
type prog = Prog of (Id.l * float) list * fundef list * t

let fletd(x, e1, e2) = Let((x, Type.Float), e1, e2)
let seq(e1, e2) = Let((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs =
  [| "%t0"; "%t1"; "%t2" ;"%t3"; "%t4"; "%t5"; "%t6"; "%t7"; "%t8"; "%t9";
     "%s0"; "%s1"; "%s2" ;"%s3"; "%s4"; "%s5"; "%s6"; "%s7";
     "%k0" |]
let fregs = 
  [| "%f1"; "%f3"; "%f4"; "%f5"; "%f6"; "%f7"; "%f8"; "%f9" ; "%f10";
     "%f11"; "%f12" ;"%f13"; "%f14"; "%f15"; "%f16"; "%f17"; "%f18"; "%f19";
     "%f20"; "%f21"; "%f22"; "%f23"; "%f24"; "%f25"; "%f26"; "%f27"; "%f28";
     "%f29"; "%f30"; "%f31" |]
let reg_cl = "%k0" (* closure address (caml2html: sparcasm_regcl) *)
let reg_sp = "%sp" (* stack pointer *)
let reg_fp = "%fp" (* frame pointer *)
let reg_hp = "%gp" (* heap pointer (caml2html: sparcasm_reghp) *)
let reg_tmp = "%at" (* assembler template *)
let reg_atmp = "%a1" (* assembler template *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)
let reg_fsw = "%f16" (* temporary for swap *)
let reg_ra = "%ra" (* return address *)
let reg_rv = "%v0" (* return value *)
let reg_frv = "%f2" (* return value *)
let reg_zero = "%zero" (* zero register *)
let reg_fz = "%f0"
let is_reg x = (x.[0] = '%' || x = reg_hp)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V(x) -> [x] | _ -> []
let rec fv_exp = function
  | Nop | Set(_) | SetL(_) | Comment(_) | Restore(_) -> []
  | Mov(x) | Neg(x) | FMovD(x) | FNegD(x) | Save(x, _) -> [x]
  | Mul(x, y') | Div(x, y') | Add(x, y') | Sub(x, y') | Ld(x, y') | LdDF(x, y') -> x :: fv_id_or_imm y'
  | St(x, y, z') | StDF(x, y, z') -> x :: y :: fv_id_or_imm z'
  | FAddD(x, y) | FSubD(x, y) | FMulD(x, y) | FDivD(x, y) -> [x; y]
  | IfEq(x, y', e1, e2) | IfLE(x, y', e1, e2) | IfGE(x, y', e1, e2) -> x :: fv_id_or_imm y' @ remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | IfFEq(x, y, e1, e2) | IfFLE(x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2) (* uniq here just for efficiency *)
  | CallCls(x, ys, zs) -> x :: ys @ zs
  | CallDir(_, ys, zs) -> ys @ zs
and fv = function
  | Ans(exp) -> fv_exp exp
  | Let((x, t), exp, e) ->
      fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans(exp) -> Let(xt, exp, e2)
  | Let(yt, exp, e1') -> Let(yt, exp, concat e1' xt e2)

let align i = i
(* let align i = (if i mod 8 = 0 then i else i + 4) *)
