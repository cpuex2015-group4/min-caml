open KNormal

(* 同じ部分式かどうか判定 *)
let rec is_common_subexpr env1 e1 env2 e2 =
  match (e1, e2) with
  | Unit, Unit -> true
  | Int(i), Int(j) -> i = j
  | Float(f), Float(g) -> f = g
  (* 識別子(Id.t)の場合は
   * - 環境に存在してその値が等しい
   * - 識別子が同じ
   * 場合に同じ部分式と判定 *)
  | Neg(x1), Neg(x2) -> is_same_id env1 x1 env2 x2
  | Add(x1, y1), Add(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | Sub(x1, y1), Sub(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | FNeg(x1), FNeg(x2) -> is_same_id env1 x1 env2 x2
  | FAdd(x1, y1), FAdd(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | FSub(x1, y1), FSub(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | FMul(x1, y1), FMul(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | FDiv(x1, y1), FDiv(x2, y2) ->
      (is_same_id env1 x1 env1 x2) && (is_same_id env1 y1 env2 y2)
  | Let((x1, t1), e11, e21), Let((x2, t2), e12, e22) ->
      (* let式の場合は定義された値を新たに環境に入れてblock内を比較する *)
      if t1 = t2 && is_common_subexpr env1 e11 env2 e12 then
        let env1' = M.add x1 e11 env1 in
        let env2' = M.add x2 e12 env2 in
        is_common_subexpr env1' e21 env2' e22
      else false
  | Var(x1), Var(x2) -> x1 = x2
  | LetRec({ name = (x1, t1); args = ys1; body = e11 }, e21),
    LetRec({ name = (x2, t2); args = ys2; body = e12 }, e22) ->
      (* let recの場合は引数を入れた環境の下で定義値を比較し、
       * 関数自体を入れた環境の下でblockを比較する *)
      if t1 = t2 && (List.map (fun (_, t) -> t) ys1) = (List.map (fun (_, t) -> t) ys2) then
        let env1' = M.add_list (List.map (fun (y,_) -> (y, Var(y))) ys1) env1 in
        let env2' = M.add_list (List.map (fun (y,_) -> (y, Var(y))) ys2) env2 in
        (is_common_subexpr env1' e11 env2' e12) &&
        (is_common_subexpr (M.add x1 e11 env1) e21 (M.add x2 e12 env2) e22)
      else false
  | App(x1, xs1), App(x2, xs2) ->
      (is_same_id env1 x1 env2 x2) && (is_same_ids env1 xs1 env2 xs2)
  | Tuple(xs1), Tuple(xs2) -> is_same_ids env1 xs1 env2 xs2
  | LetTuple(xs1, x1, e1), LetTuple(xs2, x2, e2) ->
      (is_same_ids env1 (List.map (fun (x,_) -> x) xs1) env2 (List.map (fun (x,_) -> x) xs2))
      && (is_same_id env1 x1 env2 x2) && (is_common_subexpr env1 e1 env2 e2)
  | Get(x1, y1), Get(x2, y2) ->
      (is_same_id env1 x1 env2 x2) && (is_same_id env1 y1 env2 y2)
  | Put(x1, y1, z1), Put(x2, y2, z2) ->
      (is_same_id env1 x1 env2 x2) && (is_same_id env1 y1 env2 y2) && (is_same_id env1 z1 env2 z2)
  | ExtArray(x1), ExtArray(x2) ->
      is_same_id env1 x1 env2 x2
  | ExtFunApp(x1, ys1), ExtFunApp(x2, ys2) ->
      (* 外部関数は副作用があるかどうか判定できないのでCSEを行わない *)
      false
  | _,  _ -> false
(* 同じ識別子かどうか判定 *)
and is_same_id env1 id1 env2 id2 =
  if id1 = id2 then true
  else
    try
      let e1 = M.find id1 env1 in
      let e2 = M.find id2 env2 in
      is_common_subexpr env1 e1 env2 e2
    with Not_found -> false
(* 2つのリスト内の識別子がすべて同じどうか判定 *)
and is_same_ids env1 ids1 env2 ids2 =
  if List.length ids1 != List.length ids2 then false
  else List.fold_left (&&) true (List.map2 (fun id1 id2 -> is_same_id env1 id1 env2 id2) ids1 ids2)

(* 与式が環境にある場合はその識別子を返す *)
let rec expr_of_id e env =
  let id, expr = M.choose env in
  let env' = M.remove id env in
  if is_common_subexpr env e env expr then id
  else expr_of_id e env'

(* 共通部分式除去(CSE)を行う *)
let rec g env = function
  | Unit -> Unit
  | Int(i) -> Int(i)
  | Float(f) -> Float(f)
  | Neg(x) -> Neg(x)
  | Add(x, y) -> Add(x, y)
  | Sub(x, y) -> Sub(x, y)
  | FNeg(x) -> FNeg(x)
  | FAdd(x, y) -> FAdd(x, y) 
  | FSub(x, y) -> FSub(x, y)
  | FMul(x, y) -> FMul(x, y)
  | FDiv(x, y) -> FDiv(x, y)
  | IfEq(x, y, e1, e2) -> IfEq(x, y, g env e1, g env e2)
  | IfLE(x, y, e1, e2) -> IfLE(x, y, g env e1, g env e2)
  | Let((x, t), e1, e2) -> (
      match e1 with
      | Int(_) | Float(_) ->
          (* Int, Floatは即値最適化で再配置されるのでCSEは行わない *)
          Let((x, t), e1, g env e2)
      | _ -> (
        try
          (* Letで代入している式と同じ式が環境にないか探す *)
          let id = expr_of_id e1 env in
          let e' = if id != x then (Var(id)) else e1 in
          let env' = M.add x e' env in
          Let((x, t), g env e', g env' e2)
        with Not_found ->
          (* 見つからない場合は新しく環境に登録する *)
          let env' = M.add x e1 env in
          Let((x, t), g env e1, g env' e2)))
  | Var(x) -> Var(x)
  | LetRec({ name = (x, t); args = ys; body = e1 }, e2) ->
      LetRec({ name = (x, t); args = ys; body = g env e1 }, g env e2)
  | App(x, xs) -> App(x, xs)
  | Tuple(xs) -> Tuple(xs)
  | LetTuple(xs, x, e) -> LetTuple(xs, x, g env e)
  | Get(x, y) -> Get(x, y)
  | Put(x, y, z) -> Put(x, y, z)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, ys)

let f e = g (M.empty) e
