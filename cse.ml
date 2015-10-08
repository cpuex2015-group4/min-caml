open KNormal

(* 同じ部分式かどうか判定 *)
let rec is_common_subexpr env e1 e2 =
  match (e1, e2) with
  | Unit, Unit -> true
  | Int(i), Int(j) -> i = j
  | Float(f), Float(g) -> f = g
  | Add(x1, y1), Add(x2, y2) ->
      (is_same_expr env x1 x2) && (is_same_expr env y1 y2)
  | Let((x1, t1), e11, e21), Let((x2, t2), e12, e22) ->
      if t1 = t2 && is_common_subexpr env e11 e12 then
        let env' = M.add x1 e11 (M.add x2 e12 env) in
        is_common_subexpr env' e21 e22
      else false
  | _, _ -> false
(* 同じ識別子かどうか判定 *)
and is_same_expr env id1 id2 =
  if id1 = id2 then true
  else
    try
      let e1 = M.find id1 env in
      let e2 = M.find id2 env in
      is_common_subexpr env e1 e2
    with Not_found -> false

(* 与式が環境にある場合はその識別子を返す *)
let rec expr_of_id e env =
  let id, expr = M.choose env in
  let env' = M.remove id env in
  if is_common_subexpr env e expr then id
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
  | IfEq(x, y, e1, e2) -> IfEq(x, y, e1, e2)
  | IfLE(x, y, e1, e2) -> IfEq(x, y, e1, e2)
  | Let((x, t), e1, e2) -> (
      try
        (* Letで代入している式と同じ式が環境にないか探す *)
        let id = expr_of_id e1 env in
        let env' = M.add x (Var(id)) env in
        Let((x, t), Var(id), g env' e2)
      with Not_found ->
        (* 見つからない場合は新しく環境に登録する *)
        let env' = M.add x e1 env in
        Let((x, t), g env' e1, g env' e2))
  | Var(x) -> Var(x)
  | LetRec({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = e1 }, e2)
  | App(e, es) -> App(e, es)
  | Tuple(xs) -> Tuple(xs)
  | LetTuple(xs, x, e) -> LetTuple(xs, x, e)
  | Get(x, y) -> Get(x, y)
  | Put(x, y, z) -> Put(x, y, z)
  | ExtArray(x) -> ExtArray(x)
  | ExtFunApp(x, ys) -> ExtFunApp(x, ys)

let f e = g (M.empty) e
