type ty =
  | TyVar of int * int     (* 型変数 de Bruiji(ド・ブラウン index) *)
  | TyArr of ty * ty
  | TyAll of string * ty
  | TySome of string * ty

type term =
  | TmVar of int * int
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmTAbs of string * term
  | TmTApp of term * ty
  | TmPack of ty * term * ty
  | TmUnpack of string * string * term * term


type binding =
  | NameBind
  | VarBind of ty
  | TyVarBind    (* 型変数の束縛 *)


let tmmap onvar ontype c t =
  let rec walk c t = match t with
  | TmVar(x,n) ->
    onvar c x n
  | TmAbs(x, tyT1, t2) ->
    TmAbs(x, ontype, c tyT1, walk (c+1) t2)
  | TmApp(t1, t2) ->
    TmApp(walk c t1, walk c t2)
  | TmTAbs(tyX, t2) ->
    TmTAbs(tyX, walk (c+1) t2)
  | TmTApp(t1, tyT2) ->
    TmTApp(walk c t1, ontype c tyT2)
  | TmPack(tyT1, t2, tyT3) ->
    TmPack(ontype c tyT1, walk c t2, ontype c tyT3)
  | TmUnpack(tyX, x, t1, t2) ->
    TmUnpack(tyX, x, walk c t1, walk (c+2) t2) 
  in walk c t                

let termShiftAbove d c t =
  tmmap 
    (fun c x n -> if x>=c then TmVar(x+d, n+d)
                  else TmVar(x, n+d))
    (typeShiftAbove d)
    c t
 
let termShift d t = termShiftAbove d 0 t
    

(* 型の中の変数をシフトする関数 *)  
let typeShiftAbove d c tyT =
  tymap 
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x, n+d))
    c tyT

let typeShift d ty =
  typeShiftAbove d 0 ty

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x, n)))
    j tyT

let typeSubstTop tyS tyT =
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let tymap onvar c tyT =
  let rec walk c tyT = match tyT with
  | TyArr(tyT1, tyT2) ->
    TyArr(walk c tyT1, walk c tyT2)
  | TyVar(x,n) ->
    onvar c x n
  | TyAll(tyX, tyT2) ->
    TyAll(tyX, walk (c+1) tyT2)
  | TySome(tyX, tyT2) ->
    TySome(tyX, walk (c+1) tyT2)
  in walk c tyT        
