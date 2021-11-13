open Ugraph

(* let () =
  let ic = 
    if Array.length Sys.argv = 1 then
      stdin
    else
      Stdio.In_channel.create Sys.argv.(1)
  in
  Stdio.printf "%s" @@ Stdio.In_channel.fold_lines ~init:() ~f:(fun (v1, v2, w) line ->
    ) (ic) *)
(* let split_lines *)

(*let read_file file_name =*)
(*  let ic = Stdio.In_channel.create file_name in*)
(*    Stdio.In_channel.fold_lines ~init:Ugraph.empty ~f:(fun acc line ->*)
(*      (Base.String.split line ~on:(Base.Char.of_string " "))*)
(*      |> (List.map Base.Int.of_string) |>*)
(*         Base.List.fold [] (fun acc' [v1, v2, w] -> Ugraph.add_edge (Ugraph.make_edge (v1, v2, w)) acc)) ic*)

(*

*)
let prim start_v g =
     let visited = ref [start_v];
     let mst = ref [];
     while len(visited) < len(mst) do
        (Ugraph.fold (fun x acc -> Ugraph.fold x (G.map)) g)




  (* (Base.List.fold (fun acc list -> 
    match list with
    | [] -> acc
    | [v1, v2, w] -> (Ugraph.add_edge (Ugraph.make_edge (v1 v2 w)) Ugraph.empty acc)) ) *)
