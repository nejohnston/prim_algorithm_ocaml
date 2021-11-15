open Ugraph

(*let graph_from_list list = Base.List.map (fun [v1, v2, w] -> add_edge (make_edge (v1, v2, w)) acc)*)

let line_fold acc line =
    let split = Base.String.split line ~on:(Base.Char.of_string " ") in
    let int_list = (List.map (fun x -> Base.Int.of_string x) split) in
    let string_edge = Array.of_list int_list in
        add_edge(make_edge(string_edge.(0), string_edge.(1), string_edge.(2))) acc;;

let read_file file_name =
  let ic = Stdio.In_channel.create file_name in
    Stdio.In_channel.fold_lines ~init:(empty) ~f:line_fold ic

(*let get_least edges mst =*)
(*    let rec get_least' acc =*)
(*        match acc, edges with*)
(*        | (), (v1', v2', w') -> (v1', v2', w')*)
(*        | (v1, v2, w), (v1', v2', w') when w' < w && !(List.exists (v1', v2', w') mst) then (v1', v2', w') = acc*)
(*    in*)
(*    get_least' (G.inf, G.inf, G.inf)*)

(*let get_least edges = List.hd (Base.List.sort edges ~compare:(fun (v1, v2, w) (v1', v2', w') -> Int.compare w w'))*)
(*(*(List.fold_right ~f:(fun (v1, v2, w) [(v1', v2', w')] ->*)*)
(*(*                                     if w' > w then (v1, v2, w) else (v1', v2', w')) edges [])*)*)

(*let get_edges visited g =*)
(*            List.fold_right (fun v acc ->*)
(*            (*get all neighbours of all the vertices in  visited*)*)
(*            (List.append (Base.List.map (neighbours v g) (fun (v2, w) -> (v, v2, w))) acc)*)
(*            ) visited []*)


(*let prim start_v g =*)
(*     let visited = ref [start_v] in*)
(*     let mst = ref Ugraph.empty in*)
(*     while List.length(!visited) < List.length(vertices(g)) do*)
(*        (*folding over visited, folding over the neighbours*)*)
(*        let edges = (get_edges (!visited) g)*)
(*        in*)
(*        (*iterate through edges, finding the least weight*)*)
(*        let (v1, v2, w) = (get_least edges)*)
(*        in*)
(*        mst := (add_edge (make_edge (v1, v2, w)) (!mst))*)
(*        visited := vertices (!mst)*)
(*(*        match least with*)*)
(*(*                   | () -> !(visited)*)*)
(*(*                   | (v1, v2, w) -> match (List.find_opt v1 !(visited)), (List.find_opt v2 !(visited)) with*)*)
(*(*                                    | None, None -> G.add v1 !(visited) |> G.add v2 !(visited)*)*)
(*(*                                    | Some x, None -> G.add v2 !(visited)*)*)
(*(*                                    | None, Some x -> G.add v1 !(visited)*)*)
(*        done*)

(*let rec prim' vs tree graph_edges =*)
(*        let t' = List.filter (fun (v1, v2, _) -> (List.mem v1 vs) && not (List.mem v2 vs)) graph_edges*)
(*        in*)
(*        let vs_edges = (List.sort (fun (_, _, w) (_, _, w') -> Int.compare w w') t')*)
(*        in*)
(*        let (v1_new, v2_new, w_new) = List.hd vs_edges*)
(*        in*)
(*        let new_tree = (v1_new, v2_new, w_new)::tree*)
(*        in*)
(*        let new_vs = match (List.mem v1_new vs), (List.mem v2_new vs) with*)
(*                       | true, false -> v2_new::vs*)
(*                       | _, _ -> vs*)
(*        in*)
(*        if (List.length graph_edges <> 0) then prim' new_vs new_tree (List.tl vs_edges) else new_tree*)
(* need to get all edges that are in edges parameter, where v1 is in vs, v2 is not in vs, then get smallest weight*)

let rec prim' vs tree graph_edges g_len =
        let t' = List.filter (fun (v1, v2, _) -> (List.mem v1 vs) && not (List.mem v2 vs)) graph_edges
        in
        let (v1_new, v2_new, w_new) = List.hd (List.sort (fun (_, _, w) (_, _, w') -> Int.compare w w') t')
        in
        let new_vs = v2_new::vs
        in
        let new_tree = (v1_new, v2_new, w_new)::tree
        in
        let new_edges = List.filter
            (fun (v1, v2, w) ->
            ((v1, v2, w) <> (v1_new, v2_new, w_new)) && ((v2, v1, w) <> (v2_new, v1_new, w_new))) graph_edges
        in
        if (List.length new_edges <> 0) then prim' new_vs new_tree new_edges g_len else new_tree


let prim v g =
    prim' [v] [] (edges g) (List.length (vertices(g)))

let rec rec_print list =
    match list with
    | [] -> ()
    | (v1, v2, w) :: t -> Stdio.printf "<%d, %d, %d> " v1 v2 w; rec_print t

let weights list =
    let rec aux list acc =
    match list with
    | [] -> acc
    | (_, _, w) :: t -> aux t (acc+w)
    in
    aux list 0

let () =
    let g = read_file "h.txt"
    in
    let vs = prim 1 g
    in
    Stdio.printf "minimum weight: %d\n" (weights(vs));
    rec_print vs;


