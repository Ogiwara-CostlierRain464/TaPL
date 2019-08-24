type ty = 
  | TyRecord of (string * ty) list (* {a: Top, b: Top->Top} *)
  | TyTop               
  | TyArr of ty * ty               (* A -> B *)

type term =
  | TmVar of int * int
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmRecord of (string * term) list
  | TmProj of term * string         (* recordの要素を抽出する項  t1.li *)

type binding = 
  | NameBind
  | VarBind of ty    

exception TypeError of string
let error str = raise(TypeError str)

let rec subtype tyS tyT =
  (=) tyS tyT ||
  match (tyS, tyT) with
  | (_, TyTop) ->
    true
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
    (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
  | (TyRecord(fS), TyRecord(fT)) ->
    List.for_all
     (fun (li, tyTi) ->
      (* list fS内からラベルliを検索し、それに紐付けられた要素を返す *)
      try let tySi = List.assoc li fS in
        subtype tySi tyTi
      with Not_found -> false) fT
  | (_,_) ->
    false

type context = (string * binding) list 

let getbinding ctx n = List.nth ctx n |> snd


let getTypeFromContext ctx i = 
  match getbinding ctx i with
  | VarBind(tyT) -> tyT
  | _ -> error "www"


(* 文脈の拡張 *)
let addbinding ctx x bind = 
  (x, bind)::ctx


let rec str_ty ty =
  match ty with
  | TyArr(t1,t2) -> str_ty(t1) ^ " -> " ^ str_ty(t2) 
  | TyTop -> "Top"
  | TyRecord(fields) ->
    List.fold_left (fun acc (label,ty1) -> acc ^  (label ^ ":" ^ str_ty(ty1))   ) "" fields


let rec typeof ctx t =
  match t with
  | TmRecord(fields) ->
    let fieldtys = List.map (fun (li, ti) -> (li, typeof ctx ti)) fields in
    TyRecord(fieldtys)
  | TmProj(t1, l) ->
    (match (typeof ctx t1) with
      | TyRecord(fieldtys) ->
        (try List.assoc l fieldtys 
          with Not_found -> error ("label " ^ l ^ " not found")
        )
      | _ -> error "Expected record type"  
    )  
  | TmVar(i,_) ->
    getTypeFromContext ctx i
  | TmAbs(x,tyT1,t2) ->
    let ctx' = addbinding ctx x (VarBind(tyT1)) in
    let tyT2 = typeof ctx' t2 in
    TyArr(tyT1, tyT2)
  | TmApp(t1,t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
    (match tyT1 with
    | TyArr(tyT11,tyT12) ->
      if subtype tyT2 tyT11 then tyT12
      else error "parameter type mismatch"
    | _ -> error "arrow type expected"    
  )    
    
let testcases = [
  (* (\x: Top->Top.x)(\y:Top.y) *)
  TmApp(TmAbs("x", TyArr(TyTop,TyTop),TmVar(0, 1)), TmAbs("y", TyTop, TmVar(0,1)));
  (* (\x: Record->Top.x)(\y:Top.y) *)
  (* 当然、これは部分型付けでは導出できない。 *)
  TmApp(TmAbs("x", TyArr(TyRecord([("a", TyTop)]),TyTop),TmVar(0, 1)), TmAbs("y",TyTop, TmVar(0,1)));
]  

    
(* (\x.x)(\y.y) => (\y.y) に型付けを行う  *)
let () = 
  testcases 
  |> List.iter (fun tm -> print_string(str_ty(typeof [] tm) ^ "\n"));
  ()