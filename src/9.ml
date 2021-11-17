let env_of_list list =
  if list = [] then "|- " else (String.concat ", " list) ^ " |- "


let econs list vs =
  let env = env_of_list list in
  let rec sub vs idt =
  match vs with
    | [] -> ""
    | h :: [] -> idt ^ env ^ "[] evalto [] by E-Nil {};"
    | h :: t ->
        let all = (String.concat " :: " (h :: t)) in
        idt ^ env ^ all ^ " evalto " ^ all ^ " by E-Cons {\n"
          ^ "  " ^ idt ^ env ^ h ^ " evalto " ^ h ^ " by E-Int {};\n"
          ^ (sub t (idt ^ "  ")) ^ "\n"
        ^ idt ^ "};"
  in sub vs ""

let print_econs list vs = Printf.printf "%s\n" (econs list vs)
