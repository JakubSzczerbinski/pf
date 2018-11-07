
(* ZAD 1 *)
let is_palindrom l = 
        let aux xs ys =
                match (xs, ys) with
                | _, [] -> (true, xs)
                | _, [_] -> (true, xs)
                | x::xs y::ys -> let (b, p::ps) = aux xs ys 
                        in (b && (p = x), ps)
        in aux l l;;

