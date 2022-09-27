open Sexplib.Std

type lc =
  [ `Var of string
  | `App of lc * lc
  | `Abs of string * lc ] [@@deriving sexp]

(* Polymorphic variant of lc with extra constructor *)
type lc_let = 
  [ lc
  | `Let of string * lc_let * lc_let ] [@@deriving sexp]

  
(* nanopass let desugaring *)
let rec lc_of_lc_let = function
  | #lc as lc -> lc
  | `Let (x, e1, e2) -> `App (`Abs (x, lc_of_lc_let e2), lc_of_lc_let e1)


  (* *)
let rec eval_lc (e : lc) (s : (string * lc) list) : lc =
  match e with
  | `Var x -> List.assoc x s
  | `App (e1, e2) -> eval_lc e1 s |> eval_lc e2 s
  | `Abs (x, e) -> `Abs (x, e)

let rec eval_lc_let (e : lc_let) (s : (string * lc) list) : lc =
  eval_lc (lc_of_lc_let e) s
  
(* helper stuff for S-exp conversion, can safely ignore *)
let to_hum f x = Sexplib.Sexp.to_string_hum (f x)
let lc_to_string = to_hum sexp_of_lc

let lc_from_string s = lc_of_sexp @@ Sexplib.Sexp.of_string s

let compare x y = lc_from_string x
  x
  |> lc_from_string
  |> lc_to_string
  |> String.equal y

let%test _ = compare "(Var x)" "(Var x)"

let%test _ = compare
  "(Let x (Var x) (Var y))"
  "(App (Abs x (Var y)) (Var x))"