
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let leibnitz = 
  let rec aux acc x () =
    let acc1 = acc +. (if x mod 2 = 0 then 1.0 else -1.0) *. 1.0/.(2.0 *. (float_of_int x) +. 1.0)
    in LCons (acc1, aux acc1 (x + 1))
  in aux 0.0 0 ();;

let transform ll f =
  let rec aux x1 x2 x3 ll_ ()  =
    match ll_ with
    | LNil -> LNil
    | LCons (x, xs) -> LCons (f x1 x2 x3, aux x2 x3 x (xs ()))
  in let rec aux2 l ll_ =
  match l with
  | [x1; x2; x3] -> aux x1 x2 x3 ll_ ()
  | _ -> match ll_ with
    | LNil -> LNil
    | LCons (x, xs) -> aux2 (x::l) (xs ())
  in aux2 [] ll;;
 
let f x y z = z -. (y -. z) *. (y -. z) /. (x -. 2.0 *. y +. z);;

let rec ltake = function
  (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,xf)) -> x::ltake(n-1, xf());;

type 'a llist2 = LNil2 | LCons2 of 'a * 'a llist2 Lazy.t;;


let leibnitz2 = 
  let rec aux acc x =
    let acc1 = acc +. (if x mod 2 = 0 then 1.0 else -1.0) *. 1.0/.(2.0 *. (float_of_int x) +. 1.0)
    in LCons2 (acc1, lazy (aux acc1 (x + 1)))
  in aux 0.0 0;;

let transform2 ll f =
  let rec aux x1 x2 x3 ll_  =
    match ll_ with
    | LNil2 -> LNil2
    | LCons2 (x, lazy xs) -> LCons2 (f x1 x2 x3, lazy (aux x2 x3 x xs))
  in let rec aux2 l ll_ =
  match l with
  | [x1; x2; x3] -> aux x1 x2 x3 ll_
  | _ -> match ll_ with
    | LNil2 -> LNil2
    | LCons2 (x, lazy xs) -> aux2 (x::l) (xs)
  in aux2 [] ll;;
 

let rec ltake2 = function
  (0, _) -> []
  | (_, LNil2) -> []
  | (n, LCons2(x,lazy xf)) -> x::ltake2(n-1, xf);;

type action = Fill of int | Drain of int | Transfer of int * int;;

let max a b = if a > b then a else b;;
let min a b = if a < b then a else b;;

let rec perform_fill x state capacities =
  let (b::bs, c::cs) = (state, capacities) in if x = 0 then c::bs else b::(perform_fill (x-1) bs cs);;

let rec perform_drain x state capacities =
  let (b::bs, c::cs) = (state, capacities) in if x = 0 then 0::bs else b::(perform_drain (x-1) bs cs);;

let rec perform_transfer y x state capacities =
  let rec drain y state capacities v =
    let (b::bs, c::cs) = (state, capacities) in
    if y = 0 then let nb = max (b - v) 0 in (b - nb, nb::bs)
    else let (v, s) = drain (y - 1) bs cs v in (v, b::s)
  in let rec fill x state capacities v =
    let (b::bs, c::cs) = (state, capacities) in 
    if x = 0 then let nb = min (b + v) c in (nb - b, nb::bs) 
    else let (v, s) = fill (x - 1) bs cs v in (v, b::s)
  in let rec seek x y state capacities =
    let (b::bs, c::cs) = (state, capacities) 
    in 
      if x = 0 then 
        let (v, s) = drain (y - 1) bs cs (c - b) in (v + b)::s
      else if y = 0 then
        let (v, s) = fill (x - 1) bs cs b in (b - v)::s
      else b::(seek (x - 1) (y - 1) bs cs)
  in seek x y state capacities;;

let perform action state capacities = 
  match action with
  | Fill x -> perform_fill x state capacities
  | Drain x -> perform_drain x state capacities
  | Transfer (x, y) -> perform_transfer x y state capacities;;

let actions n = 
  let rec t n = 
    let rec aux x n = if x = n then [] else (Transfer (x, n))::(Transfer (n, x))::(aux (x + 1) n)
    in if n = 0 then [] else (aux 0 n) @ (t (n - 1))
  in 
  let rec fd n = if n = 0 then [Fill n; Drain n] else (Fill n)::(Drain n)::(fd (n - 1))
  in (t n) @ (fd n);;

let rec nullstate n =
  if n = 0 then [] else 0::(nullstate (n - 1));;

let rec take n l = 
  if n = 0 then [] else let x::xs = l in x::(take (n - 1) xs);;
let sols (capacities, vol) n =
  let acts = actions (List.length capacities - 1)
  in let rec aux states n = 
    let ns = List.fold_right 
      (fun action acc -> (List.map (fun state -> let (s, h) = state in (perform action s capacities, action::h)) states) @ acc)
      acts []
    in let sols = List.map snd (List.filter (fun state -> let (s, h) = state in List.exists (fun x -> x = vol) s) ns)
    in let nn = n - List.length sols
    in if nn <= 0 then take n sols else sols @ aux ns nn
  in aux [((nullstate (List.length capacities)), [])] n;;


