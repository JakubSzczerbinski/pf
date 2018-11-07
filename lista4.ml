
(* ZAD 1 *)
let is_palindrom l = 
        let rec aux xs ys =
                match (xs, ys) with
                | _, [] -> (true, xs)
                | _, [_] -> (true, xs)
                | x::xs, _::_::ys -> 
                  let (b, p::ps) = aux xs ys 
                  in (b && (p = x), ps)
        in aux l l;;

(* ZAD 2 *)
type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let max a b = if a > b then a else b;;
let diff a b = if a > b then a - b else b - a;;
let is_balanced t =
  let rec aux t = 
    match t with
    | Leaf -> Some 0;
    | Node (l, _, r) -> 
        let hl = aux l in
        let hr = aux r in
        match (hl, hr) with
        | Some a, Some b -> if (diff a b) > 1 then None else Some (max a b)
        | None, _ -> None
        | _, None -> None
  in match (aux t) with
  | Some _ -> true
  | None -> false;;

let split t = 
  let rec aux xs ys zs = 
    match (xs, ys) with
    | _, [] -> (List.rev zs, xs)
    | _, [_] -> (List.rev zs, xs)
    | x::xs, _::_::ys -> aux xs ys (x::zs)
  in aux t t [];;

let rec make_balanced l = 
  match l with
  | [] -> Leaf
  | _ -> let (lh, r::rh) = split l in Node ((make_balanced lh), r, (make_balanced rh));;



