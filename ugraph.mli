(* abstract graph type *)
type t
(* abstract edge type *)
type edge
(* special large value returned by weight fn *)
val inf : int

(* returns an edge from the triple (v1, v2, w) indicating an edge connecting
vertices v1 and v2 with weight w *)
val make_edge : int * int * int -> edge

(* empty graph *)
val empty : t

(* adds an edge to a graph *)
val add_edge : edge -> t -> t

(* returns the list of all vertices in a graph *)
val vertices :  t -> int list

(* returns a list of all edges in a graph where an edge is represented by a
    triple of integers: (vertex1, vertex2, weight) *)
val edges : t -> (int * int * int) list

(* returns a list of all neighbours of a specific vertex in a graph;
    each pair in the list represents (neighbour_vertex, weight) *)
val neighbours : int -> t -> (int * int) list

(* [weight v1 v2 g] returns the weight of an edge betweenn vertices v1 & v2
    in graph g; it assumes that there is at most 1 eedge connecting 2 vertices;
    it returns the special value inf if there is no edge between v1 & v2 *)
val weight : int -> int -> t -> int

