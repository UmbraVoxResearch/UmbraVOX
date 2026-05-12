module Test

open FStar.UInt8
open FStar.Seq

let my_list : list UInt8.t = [0uy; 1uy; 2uy]
let my_list_len : squash (List.Tot.length my_list = 3) = assert_norm (List.Tot.length my_list = 3)

(* Does Z3 now know List.Tot.length my_list = 3? *)
let test_subtype (i : nat{i < 3}) : unit =
  let _ = List.Tot.index my_list i in
  ()
