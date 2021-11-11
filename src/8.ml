let env_of_list list =
  if list = [] then "|- " else (String.concat ", " list) ^ " |- "

let value_of_d list d = if d.[0] = '#' then List.nth (List.rev list) ((int_of_char d.[1]) - (int_of_char '1')) else d

let eval_of_d list d = d ^ " evalto " ^ (value_of_d list d) ^ " by " ^ (if d.[0] = '#' then "E-Var {};" else "E-Int {};")

let rec fill v n =
  if n = 0 then [] else v :: (fill v (n - 1))

let split e =
  let op =
    if String.contains e '+' then "+"
    else if String.contains e '-' then "-"
    else if String.contains e '*' then "*"
    else "" in
  if op = "" then [""; e; ""]
  else
    let sp = List.map (fun x -> String.trim x) (String.split_on_char op.[0] e) in
    [op; List.nth sp 0; List.nth sp 1]

let string_of_op = function
  | "+"-> "Plus"
  | "-" -> "Minus"
  | _ -> "Times"

let fun_of_op = function
  | "+" -> ( + )
  | "-" -> ( - )
  | _ -> ( * )

let value_of_e list e =
  match (split e) with
  | "" :: d :: _ -> value_of_d list d
  | op :: d1 :: d2 :: [] -> string_of_int (fun_of_op op (int_of_string (value_of_d list d1)) (int_of_string (value_of_d list d2)))
  | _ -> ""

(* d | d1 op d2 *)
let eop list e nest =
  let env = env_of_list list in
  let idt = String.concat "" (fill "  " nest) in
  let sp = split e in
  let op = List.nth sp 0 in
  let d1 = List.nth sp 1 in
  if op = "" then idt ^ env ^ (eval_of_d list d1)
  else
    let d2 = List.nth sp 2 in
    let v = value_of_e list e in
    let top = env ^ d1 ^ " " ^ op ^ " " ^ d2 ^ " evalto " ^ v ^ " by E-" ^ (string_of_op op) ^ " {" in
    let s1 = env ^ (eval_of_d list d1) in
    let s2 = env ^ (eval_of_d list d2) in
    let s3 =
      (value_of_d list d1) ^ " " ^ (String.lowercase_ascii (string_of_op op)) ^ " " ^ (value_of_d list d2)
      ^ " is " ^ v ^ " by B-" ^ (string_of_op op) ^ " {};" in
    idt ^ top ^ "\n"
      ^ idt ^ "  " ^ s1 ^ "\n"
      ^ idt ^ "  " ^ s2 ^ "\n"
      ^ idt ^ "  " ^ s3 ^ "\n"
      ^ idt ^ "};"

let print_eop list e = Printf.printf "%s\n" (eop list e 0)

(* let . = e1 in e2 *)
let elet list e1 e2 nest =
  let env = env_of_list list in
  let v1 = value_of_e list e1 in
  let l' = list @ [v1] in
  let v2 = value_of_e l' e2 in
  let s1 = (if env = "" then "|- " else env) ^ "let . = " ^ e1 ^ " in " ^ e2 ^ " evalto " ^ v2 ^ " by E-Let {" in
  let s2 = eop list e1 (nest + 1) in
  let s3 = eop l' e2 (nest + 1) in
  let idt = String.concat "" (fill "  " nest) in
  idt ^ s1 ^ "\n"
    ^ idt ^ s2 ^ "\n"
    ^ idt ^ s3 ^ "\n"
    ^ idt ^ "};"

let print_elet list e1 e2 = Printf.printf "%s\n" (elet list e1 e2 0)
