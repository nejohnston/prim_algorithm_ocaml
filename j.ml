module PrimeMap = Map.Make(Int)

let int_to_digits n =
  let open Base in
  let rec int_to_digits' num acc =
    if num < 10 then num::acc
    else int_to_digits' (num / 10) ((num % 10)::acc) in
    int_to_digits' n []
;;

let int_sort n =
  let open Base in
  let sorted_arr = List.sort ~compare:(Int.compare) (int_to_digits n)
  in
  List.fold_right sorted_arr ~f:(fun x y -> x * y * 10) ~init: 0
;;

let find_max a_map =
  PrimeMap.fold (fun key x acc -> if x > acc then x else acc) a_map 0

let maybe_read_line () =
  try Some(read_line)
  with End_of_file -> None

let () =
  let prime_mapping () =
    let rec aux acc =
      match (maybe_read_line()) with
      | Some(prime) ->
      let sorted_prime = prime |> int_of_string |> int_sort
      in
      let value = PrimeMap.find_opt sorted_prime acc
      in
      let new_value =
        match value with
        | None -> 1
        | Some v -> (v + 1)
      in
      aux (PrimeMap.add sorted_prime new_value acc)
      | None -> acc
    in
    aux PrimeMap.empty
  in
  let max = find_max (prime_mapping ())
  in
  Format.printf ("This the the greatest number of 6 digit primes: %d\n") max
