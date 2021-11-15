module G = Map.Make(Int)

type edge = (int * int * int)
type t = (int G.t) G.t

let inf = -1

let empty = G.empty

let make_edge (v1, v2, w) = (v1, v2, w)

(* adds edge to the  current graph *)
let add_edge' (v1, v2, w) g =
    match (G.find_opt v1 g) with
    | None -> g |> (G.add v1 (empty |> G.add v2 w))
    | Some x when not (G.mem v2 x) -> (G.add v1 (x |> G.add v2 w) g)
    | Some _ -> g

let add_edge (v1, v2, w) g =
    g |> add_edge' (v1, v2, w) |> add_edge' (v2, v1, w)

(*vertices need to be mapped to an array, along with the arrays that they have
    we also need to check if they're already in the accumulator*)
let vertices g =
    List.rev (G.fold (fun k _ acc ->
        (k::acc)) g [])

(*fold over outer map, enter fun, fold over inner map, finnd v2, add to acc*)
let neighbours v g =
    match (G.find_opt v g) with
    | None -> []
    | Some x -> (G.bindings x)

(*    let n = (G.find v g) in*)
(*    G.fold (fun v2' w' acc -> (v2', w')::acc) n []*)
(*    match (G.find_opt v g) with*)
(*    | None -> []*)
(*    | Some x -> G.fold (fun v2' w' acc -> (v2', w')::acc) x []*)

(*fold over outer map, enter fun, fold over inner map, create edge, add to acc*)
let edges g =
    (G.fold (fun v1 v acc -> G.fold (fun v2 w acc' -> (v1, v2, w)::acc') v acc) g [])

let weight v1 v2 g =
    try
(*        (G.find v1 g)*)
        let vertex_array = G.find v1 g
        in
        G.find v2 vertex_array
    with
        Not_found -> inf


(*#directory "_build";;*)
(*#load "ugraph.cmo";;*)
(*#show Ugraph;;*)
(*open Ugraph;;*)
