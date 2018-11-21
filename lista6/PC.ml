open Syntax

let rec string_of_prop p =
  match p with
  | Var a -> a
  | Top -> "Top"
  | Bot -> "Bot"
  | Conj (a, b) ->
      "(" ^ (string_of_prop a) ^ " and " ^ (string_of_prop b) ^ ")"
  | Disj (a, b) -> 
      "(" ^ (string_of_prop a) ^ " or " ^ (string_of_prop b) ^ ")"
  | Impl (a, b) ->
      "(" ^ (string_of_prop a) ^ " => " ^ (string_of_prop b) ^ ")"

let print_prop p =
  print_string (string_of_prop p)

let rec prove_pt pt env =
  match pt with
  | TopI -> Top
  | ConjI (left, right) ->
      let left_prop = prove_pt left env in
      let right_prop = prove_pt right env in
      Conj (left_prop, right_prop)
  | Ax prop -> 
      if List.mem prop env
      then prop
      else failwith "Failed to prove"
  | DisjIL (tree, prop) -> 
      let tree_prop = prove_pt tree env in
      Disj (tree_prop, prop)
  | DisjIR (prop, tree) ->
      let tree_prop = prove_pt tree env in
      Disj (prop, tree_prop)
  | ImplI (prop, tree) -> 
      Impl(prop, prove_pt tree (prop::env))
  | BotE prop -> prop
  | DisjE (tree, (prop1, tree1), (prop2, tree2)) ->
      let tree_prop = prove_pt tree env in
      let tree1_prop = prove_pt tree1 (prop1::env) in
      let tree2_prop = prove_pt tree2 (prop2::env) in
      if tree_prop = prop1 && tree_prop = prop2 && tree1_prop = tree2_prop
      then tree1_prop
      else failwith "Failed to prove"
  | ConjEL tree -> 
      begin match prove_pt tree env with
      | Conj (res, _) -> res
      | _ -> failwith "Failed to prove"
      end
  | ConjER tree ->
      begin match prove_pt tree env with
      | Conj (_, res) -> res
      | _ -> failwith "Failed to prove"
      end
  | ImplE (left, right) ->
      let left_prop = prove_pt left env in
      let right_prop = prove_pt right env in
      begin match right_prop with
      | Impl (premise, conclusion) -> 
          if premise = left_prop then conclusion
          else failwith "Failed to prove"
      | _ -> failwith "Faield to prove"
      end

let exists_pair pred l =
  let rec aux ll = 
    match ll with
    | x::xs -> List.exists (pred x) l || aux xs
    | [] -> false
  in aux l

let rec prove_shyp shyp env =
  let (prop, ps) = shyp in
  let proved = prove_ps ps (prop::env) in
  List.map (fun p -> Impl(prop, p)) proved

and prove_ps ps env =
  let find_premises prop =
    let find_conjunction env_prop =
      match env_prop with
      | Conj (a, b) ->
          a = prop || b = prop
      | _ -> false
    in let find_conclusions env_prop =
      match env_prop with
      | Impl (premise, conclusion) ->
          conclusion = prop
      | _ -> false
    in begin match prop with
    | Conj (a, b) ->
       List.mem a env && List.mem b env
    | Disj (a, b) ->
       List.mem a env || List.mem b env
    | (Impl _) as impl -> List.mem impl env
    | _ -> false
    end
  ||
    List.exists find_conjunction env
  ||
    let matching_implication = List.filter find_conclusions env
    in exists_pair (fun a b -> let Impl(p1, _) = a in let Impl(p2, _) = b in  List.mem (Disj(p1, p2)) env) matching_implication
    || List.exists (fun a -> let Impl(p, _) = a in List.mem p env) matching_implication
  in match ps with
  | PHyp (shyp, tail) ->
     prove_shyp shyp env 
  | PDone prop ->
      if find_premises prop then prop::env
      else 
        let _ = List.iter (fun p -> print_prop p; print_string "\n") env in
        failwith ("Failed to prove " ^ (string_of_prop prop))
  | PConc (prop, ps) ->
      if find_premises prop then prove_ps ps (prop::env)
      else
        let _ = List.iter (fun p -> print_prop p; print_string "\n") env in
        failwith ("Failed to prove " ^ (string_of_prop prop))

let prove thing =
  match thing with
  | TGoal (goal_name, goal, proof) ->
      let c = (prove_pt proof []) in
      if c = goal then
        print_string goal_name
      else
        print_prop c; print_prop goal
  | SGoal (goal_name, goal, proof) -> 
      let proved = prove_ps proof [] in  
      if List.mem goal proved then
        print_string goal_name
      else
        List.iter (fun p -> print_prop p; print_string "\n") proved;
        print_prop goal


let _ =
  let lexbuf = Lexing.from_channel stdin in
  let proofs = Parser.file Lexer.token lexbuf in
  (* powyższe wiersze wczytują listę dowodów ze standardowego wejścia
     i parsują ją do postaci zdefiniowanej w module Syntax *)
  List.iter prove proofs
