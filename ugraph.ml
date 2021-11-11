module type OrderedType =
sig
    type t
    val compare : t -> t -> int
end

module type S = sig
type t
type edge

val inf : int
val make_edge : int * int * int -> edge
val empty : t
val add_edge : edge -> t -> t
val vertices :  t -> int list
val edges : t -> (int * int * int) list
val neighbours : int -> t -> (int * int) list
val weight : int -> int -> t -> int
end

module Ugraph = struct
    type edge = (int * int * int)
    type t = Empty | Graph of t list
    
    let empty = Empty
    let make_edge v1 v2 w = v1 * v2 * w
    let add_edge edge t = 
        match t with
        | Empty -> [edge]
        | t -> edge::[t]
    let vertices t =
        let rec vertices' t acc =
            match t with
            | Empty -> acc
            | (v1, _, _) :: t -> if List.find v1 acc then vertices' t acc else vertices' t (acc::v1)
            | (_, v2, _) :: t -> if List.find v2 acc then vertices' t acc else vertices' t (acc::v2)
        in
        vertices' t []

end

