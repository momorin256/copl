let eint x =
  let sx = string_of_int x in
  sx ^ " evalto " ^ sx ^ " by E-Int {};"

(*
3 plus 5 evalto 8 by E-Plus {
  3 evalto 3 by E-Int {};
  5 evalto 5 by E-Int {};
  3 plus 5 is 8 by B-Plus {};
}
*)
let eplus_n i1 i2 v =
  let si1 = string_of_int i1 in
  let si2 = string_of_int i2 in
  let sv = string_of_int v in
  si1 ^ " + " ^ si2 ^ " evalto " ^ sv ^ " by E-Plus {\n"
    ^ "  " ^ (eint i1) ^ "\n"
    ^ "  " ^ (eint i2) ^ "\n"
    ^ "  " ^ si1 ^ " plus " ^ si2 ^ " is " ^ sv ^ " by B-Plus {};\n"
  ^ "};\n"

let eminus_n i1 i2 v =
  let si1 = string_of_int i1 in
  let si2 = string_of_int i2 in
  let sv = string_of_int v in
  si1 ^ " - " ^ si2 ^ " evalto " ^ sv ^ " by E-Minus {\n"
    ^ "  " ^ (eint i1) ^ "\n"
    ^ "  " ^ (eint i2) ^ "\n"
    ^ "  " ^ si1 ^ " minus " ^ si2 ^ " is " ^ sv ^ " by B-Minus {};\n"
  ^ "};\n"

let etimes_n i1 i2 v =
  let si1 = string_of_int i1 in
  let si2 = string_of_int i2 in
  let sv = string_of_int v in
  si1 ^ " * " ^ si2 ^ " evalto " ^ sv ^ " by E-Times {\n"
    ^ "  " ^ (eint i1) ^ "\n"
    ^ "  " ^ (eint i2) ^ "\n"
    ^ "  " ^ si1 ^ " times " ^ si2 ^ " is " ^ sv ^ " by B-Times {};\n"
  ^ "};\n"

let elt_n i1 i2 v =
  let si1 = string_of_int i1 in
  let si2 = string_of_int i2 in
  let sv = string_of_bool v in
  si1 ^ " < " ^ si2 ^ " evalto " ^ sv ^ " by E-Lt {\n"
    ^ "  " ^ (eint i1) ^ "\n"
    ^ "  " ^ (eint i2) ^ "\n"
    ^ "  " ^ si1 ^ " less than " ^ si2 ^ " is " ^ sv ^ " by B-Lt {};\n"
  ^ "};\n"


let print f i1 i2 v =
  Printf.printf "%s" (f i1 i2 v)