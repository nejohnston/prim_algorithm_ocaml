module G = Map.Make(Int)

type edge = Edge of int * int * int
type t = (int G.t) G.t

let inf = -1

let empty = G.empty

let make_edge (v1, v2, w) = Edge(v1, v2, w)

let add_edge (Edge(v1, v2, w)) g =
    match (G.find_opt v1 g) with
    | None when g = empty -> G.empty |> (G.add v1 (G.empty |> G.add v2 w))
    | None -> g |> (G.add v1 (G.empty |> G.add v2 w))
    | Some x ->
        match (G.find_opt v2 x) with
        | None -> g |> (G.add v1 (G.empty |> G.add v2 w))
        | _ -> g

(*vertices need to be mapped to an array, along with the arrays that they have
    we also need to check if they're already in the accumulator*)
let vertices g =
    List.rev (G.fold (fun k _ acc ->
        (k::acc)) g [])

(*fold over outer map, enter fun, fold over inner map, finnd v2, add to acc*)
let neighbours v g =
    (G.fold (fun v1 v acc -> G.fold (fun v2 w acc' -> (v2, w)::acc') v acc) g [])

(*fold over outer map, enter fun, fold over inner map, create edge, add to acc*)
let edges g =
    (G.fold (fun v1 v acc -> G.fold (fun v2 w acc' -> (v1, v2, w)::acc') v acc) g [])

let weight v1 v2 g =
    try
        let w = G.find v1 g
        in
        G.find v2 w
    with
        Not_found -> inf
