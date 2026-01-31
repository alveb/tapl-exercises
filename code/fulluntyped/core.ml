open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmString _  -> true
  | TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | TmFloat _  -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t) -> t
        | _ -> raise NoRuleApplies)
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi ->
          let rest' = evalafield rest in
          (l,vi)::rest'
      | (l,ti)::rest ->
          let ti' = eval1 ctx ti in
          (l, ti')::rest
      in let fields' = evalafield fields in
      TmRecord(fi, fields')
  | TmProj(fi, TmRecord(_, fields), l) ->
      (try List.assoc l fields
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmApp(fi,t1,t2) ->
      begin
          try TmApp(fi, eval1 ctx t1, t2) with NoRuleApplies ->
          try TmApp(fi, t1, eval1 ctx t2) with NoRuleApplies ->
          match t1 with
              TmAbs(_,_,t12) -> termSubstTop t2 t12
            | _ -> raise NoRuleApplies
      end
  | TmAbs(fi,x,t12) -> TmAbs(fi,x,eval1 (addname ctx x) t12)
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2)
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2' = eval1 ctx t2 in
      TmTimesfloat(fi,t1,t2')
  | TmTimesfloat(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmTimesfloat(fi,t1',t2)
  | _ ->
      raise NoRuleApplies

let eval ctx t =
  let t = ref t and i = ref 0 in
  try while true do
    t := eval1 ctx !t;
    i := !i + 1
  done
  with NoRuleApplies -> ();
  print_int !i;
  print_char ' ';
  !t

let evalbinding ctx b = match b with
    TmAbbBind(t) ->
      let t' = eval ctx t in
      TmAbbBind(t')
  | bind -> bind
