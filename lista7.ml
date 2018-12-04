

(* ZAD 1 *)
let rec fix f s = f (fix f) s;;

let fact n =
  let x = ref n in
  let v = ref 1 in
  while !x > 1 do
    v := !x * !v;
    x := !x - 1;
  done;
  !v;;

let fix_ f n = 
  let x = ref n in
  while !x != f !x do
    x := f !x
  done;
  !x;;
 
let fix_ = fix (fun fix_ -> fun f -> fun n -> let fn = f n in if n = fn then n else fix_ f (fn));;

(* ZAD 2 *)

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref;;

let rec concat_copy lhs rhs =
  match lhs with
  | LMnil -> rhs 
  | LMcons (x, xs) -> LMcons(x, ref (concat_copy !xs rhs));;

let concat_share lhs rhs =
  let rec attach_tail l =
    match l with
    | LMnil -> rhs
    | LMcons (x, ({contents = LMnil} as xs)) -> xs := rhs; lhs
    | LMcons (x, xs) -> attach_tail !xs
  in attach_tail lhs;;

let rec list_of_mutable_list lm =
  match lm with
  | LMnil -> []
  | LMcons (x, xs) -> x::(list_of_mutable_list !xs);;

let list_mutable_of_list l = List.fold_right (fun x lm -> LMcons (x, ref lm)) l LMnil;;

(* ZAD 3 *)

type ('k, 'v) memoization_dict = MDict of ('k * 'v) list

let find dict key =
  let MDict l = dict in
  let rec find l key = 
    match l with
    | (k, v)::xs -> if k = key then Some v else find xs key
    | [] -> None
  in find l key;;

let insert dict key_value =
  let MDict l = dict in MDict (key_value::l);;

let empty_dict = MDict [];;

let memo f = 
  let mdict = ref empty_dict in
  let rec mem_f a =
    match find !mdict a with
    | Some v -> v
    | None -> mdict := insert !mdict (a, f a); mem_f a
  in mem_f;;

let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);;
let mem_fib = memo fib;;
let mem_fib2 n = 
  let fib = memo mem_fib2 in
  if n <= 1 then 1 else fib(n - 1) + fib (n - 2);;

(* ZAD 4 *)

let (fresh, reset) =
  let x = ref 0
  in let fresh str =
    let t = !x in x := !x + 1;str ^ string_of_int t
  in let reset v = x := v
  in (fresh, reset);;


