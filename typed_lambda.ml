let (|>) x f = f x

type info = int  
     
(* 型の構文 T -> T or T *)
type ty =
  | TyArr of ty * ty
  | TyBool  

let print_ty ty =
  match ty with
  | TyArr(_,_) -> "TyArr"
  | _ -> "TyBool"  
  
type binding = 
  | NameBind
  | VarBind of ty  


type term =
  | TmVar of info * int * int     
  | TmAbs of info * string * ty * term  (* λ抽象   : bound_var_name * type_of_arg * partial_term *)
  | TmApp of info * term  * term        (* 関数適用 : arg_term * applying_term *)
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term   

type context = (string * binding) list

(* 文脈の拡張 *)
let addbinding ctx x bind = 
  (x, bind)::ctx

exception E of string

let error fi str = raise(E str)
let getbinding fi ctx n = List.nth ctx n |> snd

(* 文脈ctxにおいて特定の変数iに対応する型の仮定を取り出す *)
let getTypeFromContext fi ctx i = 
  match getbinding fi ctx i with
  | VarBind(tyT) -> tyT
  | _ -> error fi "www"

(* 与えられた項の型を検証する *)
let rec typeof ctx t = match t with
  | TmVar(fi, i, _) -> 
    getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
    let ctx' = addbinding ctx x (VarBind(tyT1)) in
    let tyT2 = typeof ctx' t2 in
    TyArr(tyT1, tyT2)
  | TmApp(fi,t1,t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
    (match tyT1 with
      | TyArr(tyT11, tyT12) ->
        if (=) tyT2 tyT11 then tyT12
        else error fi "parameter type mismatch"
      | _ -> error fi "arrow type expected"  
    )
  | TmTrue(fi) ->
    TyBool
  | TmFalse(fi) ->
    TyBool
  | TmIf(fi,t1,t2,t3) ->
    if (=) (typeof ctx t1) TyBool then
      let tyT2 = typeof ctx t2 in
      if (=) tyT2 (typeof ctx t3) then tyT2
      else error fi "arms of conditional have diff types"
    else error fi "guard of conditional not a boolean"  


(* (\x.x)(\y.y) => (\y.y) に型付けを行う  *)
let () = 
      let res = typeof [] (TmApp(1,
       (* \x.x *) 
       TmAbs(1, "x", TyArr(TyBool,TyBool),TmVar(1, 0, 1)),
       (* \y.y *) 
       TmAbs(1, "y",TyBool,TmVar(1, 0, 1)))
       ) in
       print_string(print_ty res);
       ()
      
      