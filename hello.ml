(* Just a stub!*)
type info = int

type term = 
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

let rec isnumericval t = match t with
  | TmZero(_) -> true
  | TmSucc(_ ,t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
  | TmTrue(_) -> true
  | TmFalse(_) -> true
  | t when isnumericval t -> true
  | _ -> false  

exception NoRuleApplies

let rec eval1 t = match t with
  | TmIf(_, TmTrue(_),t2,t3) ->
    t2
  | TmIf(_, TmFalse(_),t2,t3) -> 
    t3
  | TmIf(fi,t1,t2,t3) -> 
    let t1' = eval1 t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
    let t1' = eval1 t1 in
      TmSucc(fi, t1')
  | TmPred(_, TmZero(_)) ->
    TmZero(1)
  | TmPred(_, TmSucc(_,nv1)) when (isnumericval nv1) ->
    nv1
  | TmPred(fi,t1) ->
    let t1' = eval1 t1 in
    TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
    TmTrue(1)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval nv1) ->
    TmFalse(1)
  | TmIsZero(fi, t1) ->
    let t1' = eval1 t1 in
      TmIsZero(fi,t1')
  | _ ->
    raise NoRuleApplies                
             

let () = 
  let _ = eval1(TmIf(1,
    TmZero(1),
    TmSucc(1, TmZero(1)),
    TmPred(1, TmZero(1))
  )) in ()

