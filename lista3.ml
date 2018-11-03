open Pervasives;;
open List;;

(* ZAD 1 *)
let atoi l =
  let rec aux l num = 
    match l with
    | [] -> num
    | x::xs -> aux xs (num * 10 + (int_of_char x - int_of_char '0'))
  in aux l 0;;

let fold_atoi = List.fold_left (fun n x -> (int_of_char x - int_of_char '0') + n * 10) 0;;

(* ZAD 2 *)

let polynomial l x =
  let rec aux num l =
    match l with
    | [] -> num
    | y::ys -> aux (num *. x +. y) ys
  in aux 0.0 l;;

let fold_polynomial l x = fold_left (fun n y -> n *. x +. y) 0.0 l;;

(* ZAD 3 *)

let rec foldr f l s =
  match l with
  | [] -> s
  | x::xs -> f x (foldr f xs s);;

let fold_foldr f l s = fold_left f s (fold_left (fun acc x -> x::acc) [] l);;

(* ZAD 4 *)

let cons_all_left l x = fold_left (fun res a -> (x::a)::res) [] l;;

let ins_everywhere_left l x = 
  snd (fold_left 
    (fun res a -> let (y, ys) = res in (a::y, (x::a::y)::(cons_all_left ys a)))
    ([], [[x]])
    l);;

let cons_all_right l x = fold_right (fun a res -> (x::a)::res) l [];;

let ins_everywhere_right l x = 
  snd (fold_right
    (fun a res -> let (y, ys) = res in (a::y, (x::a::y)::(cons_all_right ys a)))
    l
    ([], [[x]]));;
(* ZAD 5 *)

let rec poly_a l x = 
  match l with
  | [] -> 0.0
  | b::bs -> b +. x *. (poly_a bs x);; 

let rec poly_b l x = fold_right (fun a num -> a +. num *. x) l 0.0;;

let rec poly_c l x =
  let rec aux l res =
    match l with
    | [] -> res
    | b::bs -> aux bs (res *. x +. b)
  in aux l 0.0;;

let rec poly_d l x = fold_left (fun num a -> a +. num *. x) 0.0 (rev l);;
(* ZAD 6 *)

let ins_everywhere_all_left l x = fold_left (fun res a -> append (ins_everywhere_left a x) res) [] l;;

let perm_left l = fold_left (fun res a -> (ins_everywhere_all_left res a)) [[]] l;;

let ins_everywhere_all_right l x = fold_right (fun a res -> append (ins_everywhere_right a x) res) l [];;

let perm_right l = fold_right (fun a res -> (ins_everywhere_all_right res a)) l [[]];;

(* ZAD 8 *)

type dim = { rows: int; columns: int };;

let mtx_dim mtx = 
  { 
    rows = (fold_left (fun x _ -> x + 1) 0 mtx);
    columns = (fold_left (fun x _ -> x + 1) 0 (if (length mtx) = 0 then [] else hd mtx))
  };;

(* ZAD 9 *)

let rec mtx_row mtx i =
  match mtx with
  | [] -> failwith "Requested row out of matrix bounds"
  | x::xs -> if i = 0 then x else mtx_row xs (i - 1);;

let rec row_elem row j = 
  match row with 
  | [] -> failwith "Requested element out row bounds"
  | x::xs -> if j = 0 then x else row_elem xs (j - 1)

let mtx_column mtx j = map (fun row -> row_elem row j) mtx;; 
let mtx_elem mtx i j = row_elem (mtx_row mtx i) j;;

(* ZAD 10 *)

let rec transpose mtx = 
  match mtx with
  | [] -> []
  | [x] -> map (fun el -> [el]) x
  | x::xs -> map (fun (el, row) -> el::row) (combine x (transpose xs));;

(* ZAD 11 *)

let mtx_add = map2 (map2 ( +. ));;

(* ZAD 12 *)

let scalar_prod a b = fold_left ( +. ) 0.0 (map (fun (ela, elb) -> ela *. elb) (combine a b));;

(* ZAD 13 *)

let mtx_apply mtx vec = map (scalar_prod vec) mtx;;
let mtx_mul m1 m2 = transpose (map (mtx_apply m1) (transpose m2));;

(* ZAD 14 *)

let rec powers_of_minus_one n =
  let rec aux i =
    if i < n then
      let power = if i mod 2 = 0 then 1.0 else -1.0 
      in power::(aux (i + 1))
    else []
  in aux 0;;

let rec drop_each mtx =
  match mtx with
  | [] -> []
  | x::xs -> xs::(map (fun a -> (x::a)) (aux xs));;

let rec drop n l =
  if n <= 0 
  then l
  else match l with
  | [] -> failwith "Cant drop more elements than on the list"
  | x::xs -> drop (n-1) xs;;

let rec det mtx =
  match mtx with
  | [[x]] -> x
  | _ ->
    let dets = map det (drop_each (map (drop 1) mtx)) in
    let summands = map2 ( *. ) dets (mtx_column mtx 0) in
    let pows = powers_of_minus_one (length summands) 
    in scalar_prod summands pows;;

