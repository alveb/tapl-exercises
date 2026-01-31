open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval t  -> true
  | _ -> false

let rec eval1 t = match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 t1 in
      TmIsZero(fi, t1')
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t

(* begin snippet 421a
let rec eval t =
  let eval1' t =
    try Some(eval1 t)
    with NoRuleApplies -> None in
  match eval1' t with
      Some(t') -> eval t'
    | None -> t
end snippet *)

(* begin snippet 421b
let eval t =
  let t = ref t in
  try while true do
    t := eval1 !t
  done
  with NoRuleApplies -> ();
  !t
end snippet *)

(* begin snippet 422
let rec eval t = match t with
    v when isval v -> v
  | TmIf(_,t1,t2,t3) -> begin match eval t1 with
      TmTrue(_) -> eval t2
    | TmFalse(_) -> eval t3
    | _ -> raise NoRuleApplies end
  | TmSucc(fi,t1) -> TmSucc(fi,eval(t1))
  | TmPred(_,t1) -> begin match eval t1 with
      TmZero(fi) -> TmZero(fi)
    | TmSucc(_,nv1) when isnumericalval nv1 -> nv1
    | _ -> raise NoRuleApplies end
  | TmIsZero(_,t1) -> begin match eval t1 with
      TmZero(fi) -> TmTrue(dummyinfo)
    | TmSucc(_,nv1) when isnumericalval nv1 -> TmFalse(dummyinfo)
    | _ -> raise NoRuleApplies end
  | _ -> raise NoRuleApplies (* Unreachable *)
end snippet *)
