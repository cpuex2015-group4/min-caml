(* type inference/reconstruction *)

open Syntax
open Exception

exception Unify of Type.t * Type.t
exception Error

let extenv = ref M.empty

(* for pretty printing (and type normalization) *)
let rec deref_typ = function (* ���ѿ�����ȤǤ���������ؿ� (caml2html: typing_deref) *)
  | Type.Fun(t1s, t2) -> Type.Fun(List.map deref_typ t1s, deref_typ t2)
  | Type.Tuple(ts) -> Type.Tuple(List.map deref_typ ts)
  | Type.Array(t) -> Type.Array(deref_typ t)
  | Type.Var({ contents = None } as r) ->
      Format.eprintf "uninstantiated type variable detected; assuming int@.";
      r := Some(Type.Int);
      Type.Int
  | Type.Var({ contents = Some(t) } as r) ->
      let t' = deref_typ t in
      r := Some(t');
      t'
  | t -> t
let rec deref_id_typ (x, t) = (x, deref_typ t)
let rec deref_term exp = 
  let (e,(token,start_p,end_p)) = exp in
  let syntax x = (x,(token,start_p,end_p)) in
  match e with
  | Not(e) -> syntax(Not(deref_term e))
  | Neg(e) -> syntax(Neg(deref_term e))
  | Add(e1, e2) -> syntax(Add(deref_term e1, deref_term e2))
  | Sub(e1, e2) -> syntax(Sub(deref_term e1, deref_term e2))
  | Eq(e1, e2) -> syntax(Eq(deref_term e1, deref_term e2))
  | LE(e1, e2) -> syntax(LE(deref_term e1, deref_term e2))
  | FNeg(e) -> syntax(FNeg(deref_term e))
  | FAdd(e1, e2) -> syntax(FAdd(deref_term e1, deref_term e2))
  | FSub(e1, e2) -> syntax(FSub(deref_term e1, deref_term e2))
  | FMul(e1, e2) -> syntax(FMul(deref_term e1, deref_term e2))
  | FDiv(e1, e2) -> syntax(FDiv(deref_term e1, deref_term e2))
  | If(e1, e2, e3) -> syntax(If(deref_term e1, deref_term e2, deref_term e3))
  | Let(xt, e1, e2) -> syntax(Let(deref_id_typ xt, deref_term e1, deref_term e2))
  | LetRec({ name = xt; args = yts; body = e1 }, e2) ->
      syntax(LetRec({ name = deref_id_typ xt;
	       args = List.map deref_id_typ yts;
	       body = deref_term e1 },
	     deref_term e2))
  | App(e, es) -> syntax(App(deref_term e, List.map deref_term es))
  | Tuple(es) -> syntax(Tuple(List.map deref_term es))
  | LetTuple(xts, e1, e2) -> syntax(LetTuple(List.map deref_id_typ xts, deref_term e1, deref_term e2))
  | Array(e1, e2) -> syntax(Array(deref_term e1, deref_term e2))
  | Get(e1, e2) -> syntax(Get(deref_term e1, deref_term e2))
  | Put(e1, e2, e3) -> syntax(Put(deref_term e1, deref_term e2, deref_term e3))
  | e -> syntax(e)

let rec occur r1 = function (* occur check (caml2html: typing_occur) *)
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Tuple(t2s) -> List.exists (occur r1) t2s
  | Type.Array(t2) -> occur r1 t2
  | Type.Var(r2) when r1 == r2 -> true
  | Type.Var({ contents = None }) -> false
  | Type.Var({ contents = Some(t2) }) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 = (* �����礦�褦�ˡ����ѿ��ؤ������򤹤� (caml2html: typing_unify) *)
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Int, Type.Int | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)));
      unify t1' t2'
  | Type.Tuple(t1s), Type.Tuple(t2s) ->
      (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify(t1, t2)))
  | Type.Array(t1), Type.Array(t2) -> unify t1 t2
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2') }) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ -> (* ������̤����η��ѿ��ξ�� (caml2html: typing_undef) *)
      if occur r1 t2 then raise (Unify(t1, t2));
      r1 := Some(t2)
  | _, Type.Var({ contents = None } as r2) ->
      if occur r2 t1 then raise (Unify(t1, t2));
      r2 := Some(t1)
  | _, _ -> raise (Unify(t1, t2))

let rec g env exp = (* �������롼���� (caml2html: typing_g) *)
  let (e,(token,start_p,end_p)) = exp in
  try
    match e with
    | Unit -> Type.Unit
    | Bool(_) -> Type.Bool
    | Int(_) -> Type.Int
    | Float(_) -> Type.Float
    | Not(e) ->
	unify Type.Bool (g env e);
	Type.Bool
    | Neg(e) ->
	unify Type.Int (g env e);
	Type.Int
    | Add(e1, e2) | Sub(e1, e2) -> (* ­�����ʤȰ������ˤη����� (caml2html: typing_add) *)
	unify Type.Int (g env e1);
	unify Type.Int (g env e2);
	Type.Int
    | FNeg(e) ->
	unify Type.Float (g env e);
	Type.Float
    | FAdd(e1, e2) | FSub(e1, e2) | FMul(e1, e2) | FDiv(e1, e2) ->
	unify Type.Float (g env e1);
	unify Type.Float (g env e2);
	Type.Float
    | Eq(e1, e2) | LE(e1, e2) ->
	unify (g env e1) (g env e2);
	Type.Bool
    | If(e1, e2, e3) ->
	unify (g env e1) Type.Bool;
	let t2 = g env e2 in
	let t3 = g env e3 in
	unify t2 t3;
	t2
    | Let((x, t), e1, e2) -> (* let�η����� (caml2html: typing_let) *)
	unify t (g env e1);
	g (M.add x t env) e2
    | Var(x) when M.mem x env -> M.find x env (* �ѿ��η����� (caml2html: typing_var) *)
    | Var(x) when M.mem x !extenv -> M.find x !extenv
    | Var(x) -> (* �����ѿ��η����� (caml2html: typing_extvar) *)
	Format.eprintf "free variable %s assumed as external@." x;
	let t = Type.gentyp () in
	extenv := M.add x t !extenv;
	t
    | LetRec({ name = (x, t); args = yts; body = e1 }, e2) -> (* let rec�η����� (caml2html: typing_letrec) *)
	let env = M.add x t env in
	unify t (Type.Fun(List.map snd yts, g (M.add_list yts env) e1));
	g env e2
    | App(e, es) -> (* �ؿ�Ŭ�Ѥη����� (caml2html: typing_app) *)
	let t = Type.gentyp () in
	unify (g env e) (Type.Fun(List.map (g env) es, t));
	t
    | Tuple(es) -> Type.Tuple(List.map (g env) es)
    | LetTuple(xts, e1, e2) ->
	unify (Type.Tuple(List.map snd xts)) (g env e1);
	g (M.add_list xts env) e2
    | Array(e1, e2) -> (* must be a primitive for "polymorphic" typing *)
	unify (g env e1) Type.Int;
	Type.Array(g env e2)
    | Get(e1, e2) ->
	let t = Type.gentyp () in
	unify (Type.Array(t)) (g env e1);
	unify Type.Int (g env e2);
	t
    | Put(e1, e2, e3) ->
	let t = g env e3 in
	unify (Type.Array(t)) (g env e1);
	unify Type.Int (g env e2);
	Type.Unit
  with Unify(t1, t2) ->
    raise (Typing_failure(
      start_p, end_p,
      Printf.sprintf "This expression has type %s but an expression was expected of type %s"
        (Type.of_string t2) (Type.of_string t1)))

let f opt e =
  extenv := M.empty;
(*
  (match deref_typ (g M.empty e) with
  | Type.Unit -> ()
  | _ -> Format.eprintf "warning: final result does not have type unit@.");
*)
  (try unify Type.Unit (g M.empty e)
  with Unify _ -> 
    if opt = "return_unit" then
      failwith "top level does not have type unit"
    else ());
  extenv := M.map deref_typ !extenv;
  deref_term e
