(* 
  #use "topfind";;
  #require "str";;
*)

let print s =
  Printf.printf "%s" s

let rec indent nest = 
  let idt = "  " in
  if nest = 0 then ""
  else idt ^ indent (nest - 1)

let string_of_env env =
  (String.concat ", " env) ^ (if env = [] then "" else " ") ^ "|- "

(* int *)
let rec is_int_of_string s =
  Printf.printf "[is int] %s\n" s;
  let is_int_of_char c = '0' <= c && c <= '9' in
  if s = "" then false
  else if String.length s = 1 then is_int_of_char s.[0]
  else
    let tail str = String.sub str 1 ((String.length str) - 1) in
    is_int_of_char s.[0] && is_int_of_string (tail s)

let deduction_of_int env i typ nest =
  Printf.printf "[int] %d %s\n" i typ;
  let idt = indent nest in
  let se = string_of_env env in
  idt ^ se ^ (string_of_int i) ^ " : int by T-Int {};\n"

(* bool *)
let is_bool_of_string s =
  Printf.printf "[is bool] %s\n" s;
  s = "true" || s = "false"

let deduction_of_bool env b typ nest =
  Printf.printf "[bool] %b %s\n" b typ;
  let idt = indent nest in
  let se = string_of_env env in
  idt ^ se ^ (string_of_bool b) ^ " : bool by T-Bool {};\n"

(* infix operator: *)
let is_plus_of_string s =
  String.contains s '+'

let is_minus_of_string s =
  String.contains s '-'

let is_times_of_string s =
  String.contains s '*'

let is_lt_of_string s =
  String.contains s '<'

let is_infix_of_string s =
  is_plus_of_string s || is_minus_of_string s || is_times_of_string s || is_lt_of_string s

let split_infix_of_string c s =
  let rec last_index ch str =
    let len = String.length str in
    if str.[len - 1] = ch then len - 1
    else last_index ch (String.sub str 0 (len - 1)) in
  let idx = last_index c s in
  (String.trim (String.sub s 0 idx), String.trim (String.sub s (idx + 1) (String.length s - idx - 1)))

(* var *)
let is_var_of_string s =
  Printf.printf "[is var] %s\n" s;
  not (is_int_of_string s) && not (is_bool_of_string s) && not (is_infix_of_string s)

let type_of_var env x =
  let p = List.find (fun s -> List.nth (String.split_on_char ':' s) 0 = x ^ " ") env in
  String.trim (List.nth (String.split_on_char ':' p) 1)

let deduction_of_var env x typ nest =
  Printf.printf "[var] %s %s\n" x typ;
  let idt = indent nest in
  let se = string_of_env env in
  let t = type_of_var env x in
  idt ^ se ^ x ^ " : " ^ t ^ " by T-Var {};\n"

(* if *)
let is_if_of_string s =
  Printf.printf "[is if] %s\n" s;
  (String.length s >= 2) && (String.sub s 0 2 = "if")

let split_if_of_string s =
  let sp = String.split_on_char '/' (let r = Str.regexp "if \\(.*\\) then \\(.*\\) else \\(.*\\)"
  in Str.global_replace r "\\1/\\2/\\3" s) in
  (List.nth sp 0, List.nth sp 1, List.nth sp 2)

(* let *)
let is_let_of_string s =
  Printf.printf "[is let] %s\n" s;
  (String.length s >= 3) && (String.sub s 0 3 = "let")

let split_let_of_string s =
  let sp = String.split_on_char '/' (let r = Str.regexp "let \\(.*\\) = \\(.*\\) in \\(.*\\)"
  in Str.global_replace r "\\1/\\2/\\3" s) in
  (List.nth sp 0, List.nth sp 1, List.nth sp 2)

(* fun *)
let is_fun_of_string s =
  Printf.printf "[is fun] %s\n" s;
  (String.length s >= 3) && (String.sub s 0 3 = "fun")

let split_fun_of_string s =
  let sp = String.split_on_char '/' (let r = Str.regexp "fun \\(.*\\) -> \\(.*\\)" in Str.global_replace r "\\1/\\2/" s) in
  (List.nth sp 0, List.nth sp 1)

let split_type_of_fun s =
  let sp = String.split_on_char '/' (let r = Str.regexp "\\(.*\\) -> \\(.*\\)" in Str.global_replace r "\\1/\\2/" s) in
  (List.nth sp 0, List.nth sp 1)

(* app *)
let is_app_of_string env s =
  Printf.printf "[is app] %s\n" s;
  let sp = String.split_on_char ' ' s in
  is_fun_of_string (List.nth sp 0)
  || (not (is_infix_of_string s)) && let r = Str.regexp "[a-z]+ .+" in Str.string_match r s 0

let split_app_of_string s =
  let sp = String.split_on_char '/' (let r = Str.regexp "\\(.*\\) \\(.*\\)" in Str.global_replace r "\\1/\\2/" s) in
  (List.nth sp 0, List.nth sp 1)

(* deduction *)
let rec type_of_string env e = 
  let type_of_if s =
    let _, _, e3 = split_if_of_string s in
    type_of_string env e3 in
  let type_of_let s =
    let _, _, e2 = split_let_of_string s in
    type_of_string env e2 in
  if is_if_of_string e then type_of_if e
  else if is_let_of_string e then type_of_let e
  else if (is_int_of_string e) || (is_plus_of_string e) || (is_minus_of_string e) || (is_times_of_string e) then "int"
  else if (is_bool_of_string e) || (is_lt_of_string e) then "bool"
  else if is_var_of_string e then type_of_var env e
  else ""

let rec type_deduction env exp typ nest =
  let deduction_of_infix env exp typ nest =
    Printf.printf "[infix] %s, %s\n" exp typ;
    let sub e1 e2 (op, sop) =
      let idt = indent nest in
      let se = string_of_env env in
      idt ^ se ^ e1 ^ " " ^ op ^ " " ^ e2 ^ " : int by T-" ^ sop ^ " {\n"
        ^ (type_deduction env e1 typ (nest + 1))
        ^ (type_deduction env e2 typ (nest + 1))
        ^ idt ^ "};\n" in
    if is_plus_of_string exp then
      let e1, e2 = split_infix_of_string '+' exp in
      sub e1 e2 ("+", "Plus")
    else if is_minus_of_string exp then
      let e1, e2 = split_infix_of_string '-' exp in
      sub e1 e2 ("-", "Minus")
    else if is_times_of_string exp then
      let e1, e2 = split_infix_of_string '*' exp in
      sub e1 e2 ("*", "Times")
    else if is_lt_of_string exp then
      let e1, e2 = split_infix_of_string '<' exp in
      sub e1 e2 ("<", "Lt")
    else ""
  in
  let deduction_of_if env exp typ nest =
    Printf.printf "[if] %s, %s\n" exp typ;
    let e1, e2, e3 = split_if_of_string exp in
    let idt = indent nest in
    let se = string_of_env env in
    idt ^ se ^ "if " ^ e1 ^ " then " ^ e2 ^ " else " ^ e3 ^ ": " ^ typ ^ " by T-If {\n"
      ^ (type_deduction env e1 typ (nest + 1))
      ^ (type_deduction env e2 typ (nest + 1))
      ^ (type_deduction env e3 typ (nest + 1))
      ^ idt ^ "};\n"
  in
  let deduction_of_let env exp typ nest =
    Printf.printf "[let] %s, %s\n" exp typ;
    let x, e1, e2 = split_let_of_string exp in
    let idt = indent nest in
    let se = string_of_env env in
    let t1 = "int -> int" in
    let t2 = typ in
    idt ^ se ^ "let " ^ x ^ " = " ^ e1 ^ " in " ^ e2 ^ " : " ^ t2 ^ " by T-Let {\n"
      ^ (type_deduction env e1 t1 (nest + 1))
      ^ (type_deduction ((x ^ " : " ^ t1) :: env) e2 t2 (nest + 1))
      ^ idt ^ "};\n"
  in
  let deduction_of_fun env exp typ nest =
    Printf.printf "[fun] %s, %s\n" exp typ;
    let x, e = split_fun_of_string exp in
    let idt = indent nest in
    let se = string_of_env env in
    let t1, t2 = split_type_of_fun typ in
    idt ^ se ^ "fun " ^ x ^ " -> " ^ e ^ " : " ^ t1 ^ " -> " ^ t2 ^ " by T-Fun {\n"
      ^ (type_deduction ((x ^ " : " ^ t1) :: env) e t2 (nest + 1))
      ^ idt ^ "};\n"
  in
  let deduction_of_app env exp typ nest =
    Printf.printf "[app] %s, %s\n" exp typ;
    let e1, e2 = split_app_of_string exp in
    let idt = indent nest in
    let se = string_of_env env in
    let t1 = type_of_string env e2 in
    let t2 = typ in
    idt ^ se ^ e1 ^ " " ^ e2 ^ " : " ^ t2 ^ " by T-App {\n"
      ^ (type_deduction env e1 (t1 ^ " ->" ^ t2) (nest + 1))
      ^ (type_deduction env e2 t1 (nest + 1))
      ^ idt ^ "};\n"
  in
  if is_if_of_string exp then deduction_of_if env exp typ nest
  else if is_let_of_string exp then deduction_of_let env exp typ nest
  else if is_fun_of_string exp then deduction_of_fun env exp typ nest
  else if is_app_of_string env exp then deduction_of_app env exp typ nest
  else if is_int_of_string exp then deduction_of_int env (int_of_string exp) typ nest
  else if is_bool_of_string exp then deduction_of_bool env (bool_of_string exp) typ nest
  else if is_infix_of_string exp then deduction_of_infix env exp typ nest
  else if is_var_of_string exp then deduction_of_var env exp typ nest
  else ""
