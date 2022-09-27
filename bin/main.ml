open Nanopass_lib.Lc

let () = 
  (* "(Var x)" *)
  (* |> lc_from_string *)
 print_string @@ lc_to_string @@ lc_of_lc_let (`Let ("x", (`Var "y"), (`Var "x")))

(* (let x = y in x) *)
(* ((\x.x) y)*)