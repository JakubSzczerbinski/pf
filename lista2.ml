
(* ZAD 1 *)

let rec append_to_all el lists =
  match lists with
  | [] -> []
  | hd :: tl -> (el :: hd)::(append_to_all el tl);;

let rec concat l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> hd::(concat tl l2);;

let rec sublists list = 
  match list with
  | [] -> [[]]
  | hd :: tl -> let subs = sublists tl in concat subs (append_to_all hd subs);;

(* ZAD 2 *)

let rec take l n = 
  if n = 0 then []
  else match l with
  |  [] -> failwith "Error"
  |  hd :: tl -> hd :: (take tl (n-1));;

let rec tail l n =
  if n = 0 then l
  else match l with
  | [] -> failwith "Error"
  | hd :: tl -> tail tl (n-1);;

let rec cycle l n = concat (tail l n) (take l n);;

(* ZAD 3 *)

let rec merge compare l1 l2 = 
  match l1 with
  | [] -> l2
  | hd1 :: tl1 -> match l2 with 
    | [] -> l1
    | hd2 :: tl2 -> if compare hd1 hd2 
      then hd1 :: (merge compare tl1 l2)
      else hd2 :: (merge compare l1 tl2);;

let rec reverse_ l res = match l with
  | [] -> res
  | hd :: tl -> reverse_ tl (hd :: res);;
let reverse l = reverse_ l [];;

let rec merget compare res l1 l2 =
  match l1 with
  | [] -> concat (reverse res) l2
  | hd1 :: tl1 -> match l2 with 
    | [] -> concat (reverse res) l1
    | hd2 :: tl2 -> if compare hd1 hd2 
      then (merget compare (hd1::res) tl1 l2)
      else (merget compare (hd2::res) l1 tl2);;
let rec merge2 compare l1 l2 = merget compare [] l1 l2;;

let rec split l = 
  match l with 
  | [] -> ([], [])
  | hd :: [] -> ([hd], [])
  | hd1 :: (hd2 :: tl) -> let (l1, l2) = split tl in ((hd1::l1), (hd2::l2));;

let rec mergesort compare l = 
  match l with
  | [] -> []
  | hd :: [] -> [hd]
  | hd1 :: (hd2 :: tl) -> let (l1, l2) = split l in merge compare (mergesort compare  l1) (mergesort compare l2);;

(* ZAD 4 *)
let rec partition_ pred l l1 l2 =
  match l with
  | [] -> (reverse l1, reverse l2)
  | hd :: tl -> if pred hd then partition_ pred tl (hd::l1) l2 else partition_ pred tl l1 (hd::l2);;
let partition pred l = partition_ pred l [] [];;

let rec quicksort_ comp l res = 
  match l with
  | [] -> res
  | [hd] -> hd :: res
  | hd :: tl -> 
      let pred = comp hd in
      let (l1, l2) = partition pred tl in
      let s1 = quicksort_ comp l1 res in quicksort_ comp l2 (hd::s1);;
let quicksort comp l = quicksort_ comp l [];;

(* ZAD 5 *)
let rec map f l = match l with
| [] -> []
| hd::tl -> (f hd)::(map f tl);;

let rec map_all f l = match l with
| [] -> []
| hd::tl -> concat (f hd) (map_all f tl);;
let rec append_to_a_place elem l =
  match l with
  | [] -> [[elem]]
  | hd::tl -> (elem::l)::(map (fun t -> hd::t) (append_to_a_place elem tl));;
let rec perm l = 
  match l with
  | [] -> [[]]
  | hd::tl -> let perm = perm tl in map_all (append_to_a_place hd) perm;;

(* ZAD 6 *)

let rec suff l = 
  match l with
  | [] -> [[]]
  | hd::tl -> l::(suff tl);;
let rec pref l = map reverse (suff (reverse l));;

