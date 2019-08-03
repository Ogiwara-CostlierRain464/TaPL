(* Utilities *)
let (|>) x f = f x
let (|-) f g = fun x -> g (f x)
let getOrElse defVal = function
  | Some(v) -> v
  | None    -> defVal

(* Term Definition *)
type term =
  | TmVar of int    * int     (* 変数      : deBruijn_index * term_length   *)
  | TmAbs of string * term    (* ラムダ抽象: bound_var_name * partial_term  *)
  | TmApp of term   * term    (* 関数適用  : arg_term       * applying_term *)
  | TmWrong                   (* 不正な項 (エラー処理用) *)


(* context: 特定のラムダ抽象における自由変数のリスト *)
type binding = NameBind
type context = (string * binding) list

(* context の長さはすなわちリストの長さ *)
let ctxlength = List.length

(* 特定の context での deBruijn インデックスから変数名を求める *)
let index2name ctx n =
  if n < ctxlength ctx then
    let name = List.nth ctx n |> fst in
    Some(name)
  else
    None

(* 標準の List.find のオプション版 *)
let tryo f x = try Some(f x) with _ -> None
let listfind p = tryo (List.find p)

(* 現在の変数名を含む新しい context を作成して返す。
 * 変数名が既に存在する場合は重複がなくなるまで ' をつけた変数名にする *)
let rec pickfreshname ctx name =
  let oldname = listfind (fst |- ((=) name)) ctx in
  match oldname with
  | Some(name, _) -> pickfreshname ctx (name ^ "'")
  | None          -> let bind = (name, NameBind) in (bind::ctx, name)

(* 項の文字列表現。TaPL の printtm の純粋関数バージョン(単純に文字列を返す) *)
let rec string_of_term ctx = function
  | TmVar(x, n) ->
      if ctxlength ctx = n then
        index2name ctx x |> getOrElse "[bad index]"
      else
        "[bad index]"

  | TmAbs(name, tm) ->
      let (ctx', name') = pickfreshname ctx name in
      "(λ " ^ name' ^ ". " ^ (string_of_term ctx' tm) ^ ")"
  | TmApp(t1, t2) ->
      "(" ^ (string_of_term ctx t1) ^ " " ^ (string_of_term ctx t2) ^ ")"
  | TmWrong -> "[Wrong Evaluation]"

(* シフト演算。TaPL の定義 6.2.1 を書き起こしただけ
 * ただし、シフトすると文脈の長さもシフトの分増える *)
let term_shift d t =
  let rec walk c = function
  | TmVar(x, ctxlen) when x < c -> TmVar(x, ctxlen+d)
  | TmVar(x, ctxlen)            -> TmVar(x+d, ctxlen+d)
  | TmAbs(name, t)              -> TmAbs(name, walk (c+1) t)
  | TmApp(t1, t2)               -> TmApp(walk c t1, walk c t2)
  | TmWrong                     -> TmWrong
  in walk 0 t

(* 代入。これも定義 6.2.4 とほとんど一緒 *)
let rec term_subst j s = function
  | TmVar(k, _) when k = j -> s
  | TmVar(_, _) as k       -> k
  | TmAbs(name, t)         -> TmAbs(name, term_subst (j+1) (term_shift 1 s) t)
  | TmApp(t1, t2)          -> TmApp(term_subst j s t1, term_subst j s t2)
  | TmWrong                -> TmWrong

(* ベータ簡約。E-AppAbs 参照 *)
let term_subst_top s t =
  let s' = term_shift 1 s in
  let t' = term_subst 0 s' t in
  term_shift (-1) t'

(* 値とはラムダ抽象のこと *)
let rec isval _ = function
  | TmAbs(_, _) -> true
  | _           -> false

(* 1 ステップ評価。図 5−3 参照
 * 関数適用以外を評価しようとすると TmWrong になる *)
let rec eval1 ctx = function
  | TmApp(TmAbs(_, t12), v2) when isval ctx v2 ->
      term_subst_top v2 t12

  | TmApp(v1, t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(v1, t2')

  | TmApp(t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(t1', t2)

  | _ -> TmWrong

(* 他ステップ評価。これ以上簡約できなくなるまで 1 ステップ評価を繰り返す *)
let rec eval ctx t =
  let t' = eval1 ctx t in
  match t' with
  | TmWrong -> t
  | _       -> eval ctx t'

(* テスト *)
let testcases = [
  (* (λ x. x) λ y. y => λ y. y *)
  (TmApp(TmAbs("x", TmVar(0, 1)), TmAbs("y", TmVar(0, 1))),                 TmAbs("y", TmVar(0, 1)));

  (* (λ x. λ y. x) λ z. z => λ y. λ z. z *)
  (TmApp(TmAbs("x", TmAbs("y", TmVar(1, 2))), TmAbs("z", TmVar(0, 1))),     TmAbs("y", TmAbs("z", TmVar(0, 2))));

  (* (λ x. λ x'. x) (λ x. x) => λ x'. λ x. x *)
  (TmApp(TmAbs("x", TmAbs("x", TmVar(1, 2))), TmAbs("x", TmVar(0, 1))),     TmAbs("x", TmAbs("x", TmVar(0, 2))));

  (* (λ x. λ y. x) (λ z. z) (λ w. w) => (λ y. λ z. z) (λ w. w) *)
  (TmApp(TmApp(TmAbs("x", TmAbs("y", TmVar(1, 2))), TmAbs("z", TmVar(0, 1))), TmAbs("w", TmVar(0, 1))),
                                                                            TmApp(TmAbs("y", TmAbs("z", TmVar(0, 2))), TmAbs("w", TmVar(0, 1))));

  (* (λ x. x) ((λ y. y) (λ z. z))  => (λ x. x) (λ z. z) *)
  (TmApp(TmAbs("x", TmVar(0, 1)), TmApp(TmAbs("y", TmVar(0, 1)), TmAbs("z", TmVar(0, 1)))),
                                                                            TmApp(TmAbs("x", TmVar(0, 1)), TmAbs("z", TmVar(0, 1))));

  (* (λ x. x x) (λ x. x x) => (λ x. x x) (λ x. x x) *)
  TmApp(TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))), TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1)))),
                                                                            TmApp(TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))), TmAbs("x", TmApp(TmVar(0, 1), TmVar(0, 1))));


] ;;

testcases
  |> List.iteri (fun i -> fun (tsrc, texpect) ->
      let tactual = eval1 [] tsrc in
      let result = if (tactual = texpect) then "OK" else "**FAILURE**" in
      Printf.printf "%d. %s\n  =>  %s\n%s\n\n" (i+1) (string_of_term [] tsrc) (string_of_term [] tactual) result;
    )