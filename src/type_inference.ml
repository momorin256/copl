(* debug print *)
let debug = false

let trace0 s =
  if debug then Printf.printf s
  else ()

let trace1 s a1 =
  if debug then Printf.printf s a1
  else ()

let trace2 s a1 a2 =
  if debug then Printf.printf s a1 a2
  else ()

let trace3 s a1 a2 a3 =
  if debug then Printf.printf s a1 a2 a3
  else ()

(* utility *)
let skip_string str n =
  let len = String.length str in
  String.sub str n (len - n)

let rec list_of_string str =
  if String.length str = 0 then []
  else str.[0] :: list_of_string (String.sub str 1 (String.length str - 1))

let contains_string str s =
  let len = String.length s in
  let rec sub i =
    if i + len > String.length str then false
    else
      if String.sub str i len = s then true
      else sub (i + 1)
  in sub 0

let split_string str s =
  let len = String.length s in
  let rec sub i =
    if i + len > String.length str then raise (invalid_arg (Printf.sprintf "%s, %s" str s))
    else
      if String.sub str i len = s then i
      else sub (i + 1)
  in
  let idx = sub 0 in
  (String.sub str 0 idx, String.sub str (idx + len) ((String.length str) - len - idx))

let print_env env =
  let rec sub = function
    | [] -> ""
    | (l, r) :: t -> (Printf.sprintf "(%s,%s)" l r) ^ (sub t)
  in
  trace1 "%s\n" (sub env)

(* extract *)
let rec extract env exp =
  (* int *)
  let is_int exp = List.for_all (fun c -> '0' <= c && c <= '9') (list_of_string exp)
  in
  let extract_int env i =
    trace1 "[int] %d\n" i;
    ([], "int")
  in

  (* bool *)
  let is_bool exp = (exp = "true") || (exp = "false")
  in
  let extract_bool env b =
    trace1 "[bool] %b\n" b;
    ([], "bool")
  in

  (* var *)
  let is_var exp = (not (is_int exp)) && (not (is_bool exp))
  in
  let extract_var env x =
    trace1 "[var beg] %s\n" x;
    let rec find_var x = function
      | [] -> None
      | (name, typ) :: t -> if x = name then (Some typ) else find_var x t
    in
    let t =
      let typ = find_var x env in
      match typ with
      | None -> "a_" ^ x
      | Some v -> v
    in
    trace1 "[var end] %s\n" t;
    ([], t)
  in

  (* infix *)
  let is_infix exp =
    String.contains exp '+' || String.contains exp '-' || String.contains exp '*' || String.contains exp '<'
  in
  let split_infix exp =
    let rec rev_string str =
      if String.length str = 0 then ""
      else
        let idx = (String.length str) - 1 in
        String.make 1 str.[idx] ^ (rev_string (String.sub str 0 idx))
    in
    let op =
      if String.contains exp '+' then "+"
      else if String.contains exp '-' then "-"
      else if String.contains exp '*' then "*"
      else if String.contains exp '<' then "<"
      else "?"
    in
    let len = String.length exp in
    let idx = len - 1 - String.index (rev_string exp) op.[0] in
    let rem = String.length exp - idx - 1 in
    (String.trim (String.sub exp 0 idx), String.trim (String.sub exp (idx + 1) rem), op)
  in
  let extract_infix e1 e2 op env =
    let env1, t1 = extract env e1 in
    let env2, t2 = extract env e2 in
    let env3 = env1 @ env2 in
    if op = "<" then (env3, "bool") else (env3, "int")
  in

  (* if *)
  let is_if exp =
    String.length exp > 2 && String.sub exp 0 2 = "if"
    in
  let split_if exp =
    let sp = String.split_on_char '/' (
      let r = Str.regexp "if \\(.+\\) then \\(.+\\) else \\(.+\\)" in
      Str.global_replace r "\\1/\\2/\\3" exp) in
    if List.length sp = 3 then (List.nth sp 0, List.nth sp 1, List.nth sp 2)
    else raise (Invalid_argument (Printf.sprintf "%s" exp))
  in
  let extract_if e1 e2 e3 =
    trace3 "[if beg] %s, %s, %s\n" e1 e2 e3;
    let env1, t1 = extract env e1 in
    let env2, t2 = extract env e2 in
    let env3, t3 = extract env e3 in
    let env4 = env1 @ env2 @ env3 @ [(t1, "bool"); (t2, t3)] in
    trace2 "[if end] %d, %s\n" (List.length env4) t2;
    (env4, t2)
  in

  (* let *)
  let is_let exp =
    String.length exp > 3 && String.sub exp 0 3 = "let"
  in
  let split_let exp =
    let sp = String.split_on_char '/' (
      let r = Str.regexp "let \\(.+\\) = \\(.+\\) in \\(.+\\)" in
      Str.global_replace r "\\1/\\2/\\3" exp) in
    if List.length sp = 3 then (List.nth sp 0, List.nth sp 1, List.nth sp 2)
    else raise (Invalid_argument (Printf.sprintf "%s" exp))
  in
  let extract_let x e1 e2 =
    trace3 "[let beg] %s, %s, %s\n" x e1 e2;
    let env1, t1 = extract env e1 in
    let env2, t2 = extract ((x, t1) :: env) e2 in
    let env3 = env1 @ env2 in
    trace2 "[let end] %d, %s\n" (List.length env3) t2;
    (env1 @ env2, t2)
  in

  (* fun *)
  let is_fun exp =
    String.length exp > 3 && String.sub exp 0 3 = "fun"
  in
  let split_fun exp =
    let head, tail = split_string exp "->" in
    (String.trim (skip_string head 3), String.trim tail)
  in
  let extract_fun x e1 =
    trace2 "[fun beg] %s, %s\n" x e1;
    let a = "a_" ^ x in
    let env1, t0 = extract ((x, a) :: env) e1 in
    let t = a ^ " -> " ^ t0 in
    trace2 "[fun end] %d, %s\n" (List.length env1) t;
    (env1, t)
  in

  (* app *)
  let is_app exp =
    let sp = String.split_on_char ' ' exp in
    if List.length sp = 2 then
      let f = List.nth sp 0 in
      let x = List.nth sp 1 in
      "a" <= f && f <= "z" && (is_int x || is_bool x) (* [a-z] [int|bool]*)
    else false
  in
  let split_app exp =
    let sp = String.split_on_char ' ' exp in
    (List.nth sp 0, List.nth sp 1)
  in
  let extract_app e1 e2 =
    trace2 "[app beg] %s, %s\n" e1 e2;
    let env1, t1 = extract env e1 in
    let env2, t2 = extract env e2 in
    let a = "a_" ^ "z" in
    let env3 = env1 @ env2 @ [(t1, t2 ^ " -> " ^ a)] in
    trace2 "[app end] %d, %s\n" (List.length env3) a;
    (env3, a)
  in

  (* main *)
  if is_if exp then
    let e1, e2, e3 = split_if exp in
    extract_if e1 e2 e3
  else if is_let exp then
    let x, e1, e2 = split_let exp in
    extract_let x e1 e2
  else if is_fun exp then
    let x, e = split_fun exp in
    extract_fun x e
  else if is_app exp then
    let e1, e2 = split_app exp in
    extract_app e1 e2
  else if is_infix exp then
    let e1, e2, op = split_infix exp in
    extract_infix e1 e2 op env
  else if is_int exp then extract_int env (int_of_string exp)
  else if is_bool exp then extract_bool env (bool_of_string exp)
  else if is_var exp then
    extract_var env exp
  else ([], "")

(* unify *)
let rec unify env =
  print_env env;

  let is_defined typ =
    let is_unit typ =
      (typ = "int") || (typ = "bool")
    in
    (is_unit typ)
    || (contains_string typ " -> ")
      && (let h, t = split_string typ " -> " in (is_unit h) && (is_unit t))
  in
  let assign env a typ =
    List.map (fun (l, r) ->
      let reg = Str.regexp a in
      (Str.global_replace reg typ l, Str.global_replace reg typ r)) env
  in
  let is_solvable (l, r) env =
    if l = r then true
    else if is_defined l then List.length (List.find_all (fun (l', r') -> contains_string l' r || contains_string r' r) env) > 1
    else if is_defined r then List.length (List.find_all (fun (l', r') -> contains_string l' l || contains_string r' l) env) > 1
    else false
  in
  let is_solved = List.length (List.find_all (fun t -> is_solvable t env) env) = 0 in
  if is_solved then env
  else
    let l, r = List.find (fun t -> is_solvable t env) env in
    if l = r then unify (List.filter (fun x -> x <> (l, r)) env)
    else if is_defined l then unify ((r, l) :: (assign (List.filter (fun t -> t <> (l, r)) env) r l))
    else if is_defined r then unify ((l, r) :: (assign (List.filter (fun t -> t <> (l, r)) env) l r))
    else []

(* solve *)
let solve env exp =
  let build_type typ env =
    let rec sub s = function
      | [] -> s
      | (a, t) :: tail ->
          sub (let reg = Str.regexp a in
          if contains_string t " -> " then Str.global_replace reg (Printf.sprintf "(%s)" t) s
          else Str.global_replace reg t s) tail (* replace a to t in typ *)
    in sub typ env
  in
  let eqs, typ = extract env exp in
  let unified = unify eqs in
  build_type typ unified
