
module type PQUEUE =
sig
        type priority
        type 'a t

        exception EmptyPQueue

        val empty : 'a t
        val insert : 'a t -> priority -> 'a -> 'a t
        val remove : 'a t -> priority * 'a * 'a t
end

module type ORDTYPE =
sig
        type t
        type comparision = LT | EQ | GT

        val compare : t -> t -> comparision
end

let min a b = if a > b then b else a;;

module PQueue (Priority: ORDTYPE) : PQUEUE with type priority = Priority.t =
struct
        type priority = Priority.t
        type 'a bucket = priority * 'a list
        type 'a t = 'a bucket list

        exception EmptyPQueue
        exception InternalError
        let empty = []
        let rec insert q p e =
                match q with
                | (pr, bucket)::xs -> 
                        begin match Priority.compare p pr with
                        | GT -> (p, [e])::q
                        | EQ -> (pr, e::bucket)::xs
                        | LT -> (pr, bucket)::(insert xs p e)
                        end
                | [] -> [(p, [e])]

        let rec remove q =
                match q with
                | [] -> raise EmptyPQueue
                | (pr, [e])::xs -> (pr, e, xs)
                | (pr, e::bucket)::xs -> (pr, e, (pr, bucket)::xs)
                | _ -> raise InternalError
end

module IntOrd : ORDTYPE with type t = int =
struct
        type t = int
        type comparision = LT | EQ | GT

        let compare a b =
                if a > b then GT
                else if a == b then EQ else LT
end

module IntQ = PQueue(IntOrd)

let sort l = 
        let rec aux l q =
                match l with
                | x::xs -> let res, q = aux xs (IntQ.insert q x ()) in let p, (), _q = IntQ.remove q in p::res, _q
                | [] -> [], q
        in fst (aux l IntQ.empty);;

