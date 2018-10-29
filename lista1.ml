
(* HELPERS *)

open StdLabels.List
let time f x =
  let start = Unix.gettimeofday ()
  in let res = f x
  in let stop = Unix.gettimeofday ()
  in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
  in res

(* ZAD 1 *)

let exp1 = fun x -> x * 1;;
let exp2 a = failwith "failed";;
let exp3 a b c = a (b c);;

(* ZAD 2 *)

let rec a_1 n =
  if n = 0 then 0
  else 2 * a_1(n-1) + 1
;;


let rec a_2_ n x acc = 
  if x = n then acc
  else a_2_ n (x + 1) (2 * acc + 1)
;;

let a_2 n = a_2_ n 0 0;;

(* ZAD 3 *)

let composition g f = fun x -> f (g x);;
let rec iter f n = if n = 0 then fun x -> x else composition f (iter f (n-1));;
let mult a b = iter ((+) a) b 0;;
let power a b = iter (( * )a ) b 1;;

(* ZAD 4 *)
let hd stream = stream 0;;
let tl stream = fun x -> stream (x + 1);;
let add stream constant = fun x -> constant + (stream x);;
let map stream f = fun x -> f (stream x);;
let map2 s1 s2 f = fun x -> f (s1 x) (s2 x);;
let replace stream a n = fun x -> if x = n then a else stream x;;
let take stream n = fun x -> stream (x * n);;
let scan stream f a = 
  let rec partial x = 
    if x == 0 then f a (stream 0)
    else f (partial x - 1) (stream x)
  in partial;;

let rec tabulate ?a:(a1=0) b1 stream = 
  if a1 = b1 then [stream a1]
  else (stream a1)::(tabulate ~a:(a1 + 1) b1 stream);;

let val_stream a = fun x -> a;;
let list_stream l = fun x -> nth l (x mod length l);;

(* ZAD5 *)

let ctrue t f = t;;
let cfalse t f = f;;

let cand lhs rhs t f = rhs (lhs t f) f;;
let cor lhs rhs t f = rhs t (lhs t f);;
let neg cbool t f = cbool f t;;

let c_bool_of_bool boolean t f = 
  if boolean then t else f;;

let bool_of_cbool cbool = cbool true false;;

(* ZAD6 *)

let zero inc zero_el = zero_el;;

let succ number inc zero_el = inc (number inc zero_el);; 

let add lhs rhs inc zero_el = lhs inc (rhs inc zero_el);;
let mul lhs rhs inc zero_el = lhs (rhs inc) zero_el;;

let isEven num = num neg ctrue;;
let isZero num t f = num (fun x -> f) t;;
let cnum_of_int n f z = iter f n z ;;
let int_of_cnum cnum = cnum (fun x -> x + 1) 0;;
