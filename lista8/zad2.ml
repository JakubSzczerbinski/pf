module type VERTEX =
sig
        type t
        type label

        val eq : t -> t -> bool
        val create : label -> t
        val label : t -> label 
end

module type EDGE =
sig
        type t
        type vertex
        type label

        val eq : t -> t -> bool
        val create : label -> vertex -> vertex -> t
        val label : t -> label
        val start_vertex : t -> vertex
        val end_vertex : t -> vertex
end


module Vertex : VERTEX with type label = String.t =
struct
        type label = String.t
        type t = Vertex of label

        let eq (Vertex a) (Vertex b) = String.equal a b
        let create str = Vertex str
        let label (Vertex str) = str
end

module Edge : EDGE with type label = String.t and type vertex = Vertex.t =
struct
        module V = Vertex
        type vertex = V.t
        type label = String.t
        type t = Edge of label * vertex * vertex

        let eq (Edge (l1, vs1, ve1)) (Edge (l2, vs2, ve2)) =
                (String.equal l1 l2) && (V.eq vs1 vs2) && (V.eq ve1 ve2)
        let create l vs ve = Edge (l, vs, ve)
        let label (Edge (l, _, _)) = l
        let start_vertex (Edge (_, vs, _)) = vs
        let end_vertex (Edge (_, _, ve)) = ve
end

module type GRAPH = 
sig
        type t
        module V : VERTEX
        type vertex = V.t

        module E : EDGE with type vertex = vertex
        type edge = E.t
        val mem_v : t -> vertex -> bool
        val mem_e : t -> edge -> bool
        val mem_e_v : t -> vertex -> vertex -> bool
        val find_e : t -> vertex -> vertex -> edge option
        val succ : t -> vertex -> vertex list
        val pred : t -> vertex -> vertex list
        val succ_e : t -> vertex -> edge list
        val pred_e : t -> vertex -> edge list
        (* funkcje modyfikacji *)
        val empty : t
        val add_e : t -> edge -> t
        val add_v : t -> vertex -> t
        val rem_e : t -> edge -> t
        val rem_v : t -> vertex -> t
        (* iteratory *)
        val fold_v : ( vertex -> 'a -> 'a ) -> t -> 'a -> 'a
        val fold_e : ( edge -> 'a -> 'a ) -> t -> 'a -> 'a
end

module Graph : GRAPH =
struct
        module V = Vertex
        module E = Edge
        type vertex = V.t
        type edge = E.t
        type t = Graph of edge list * vertex list

        let out_edges (Graph (es, vs)) v =
               List.filter (fun e -> V.eq (E.start_vertex e) v) es

        let in_edges (Graph (es, vs)) v =
               List.filter (fun e -> V.eq (E.end_vertex e) v) es

        let mem_v (Graph (es, vs)) v =
               let rec aux = function
               | [] -> false
               | x::xs -> if V.eq v x then true else aux xs
               in aux vs
        let mem_e (Graph (es, vs)) e =
               let rec aux = function
               | [] -> false
               | x::xs -> if E.eq e x then true else aux xs
               in aux es
        let mem_e_v (Graph (es, vs)) vs ve =
               let rec aux = function
               | [] -> false
               | x::xs -> if (V.eq (E.start_vertex x) vs) && (V.eq (E.end_vertex x) ve) then true else aux xs
               in aux es
        let find_e (Graph (es, vs)) vs ve =
               let rec aux = function
               | [] -> None
               | x::xs -> if (V.eq (E.start_vertex x) vs) && (V.eq (E.end_vertex x) ve) then Some x else aux xs
               in aux es
        let succ g v = List.map E.end_vertex (out_edges g v)
        let pred g v = List.map E.start_vertex (in_edges g v)
        let pred_e = in_edges
        let succ_e = out_edges 

        let empty = Graph ([], [])
        let add_e (Graph (es, vs)) e = Graph (e::es, vs)
        let add_v (Graph (es, vs)) v = Graph (es, v::vs)
        let rem_e (Graph (es, vs)) e = 
                let nes = List.filter (fun e_ -> not (E.eq e_ e)) es in Graph (nes, vs)
        let rem_v (Graph (es, vs)) v =
                let nes = 
                        List.filter
                        (fun e -> not ((V.eq v (E.start_vertex e)) || (V.eq v (E.end_vertex e))))
                        es
                and nvs = List.filter (fun v_ -> not (V.eq v_ v)) vs
                in Graph (nes, nvs)

        let fold_v f (Graph (es, vs)) a = List.fold_right f vs a
        let fold_e f (Graph (es, vs)) a = List.fold_right f es a
end
