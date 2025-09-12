let create_list n =
  let rec aux acc i =
    if i > n then acc
    else aux (i :: acc) (i + 1)
  in
  aux [] 1

let sum_list lst =
  List.fold_left (+) 0 lst

let map_double lst =
  List.map (fun x -> x * 2) lst

let filter_even lst =
  List.filter (fun x -> x mod 2 = 0) lst

let () =
  let n = 100000 in
  let start_time = Sys.time () in
  
  let lst = create_list n in
  let doubled = map_double lst in
  let evens = filter_even doubled in
  let total = sum_list evens in
  
  let end_time = Sys.time () in
  Printf.printf "List operations on %d elements\n" n;
  Printf.printf "Sum of even doubled numbers: %d\n" total;
  Printf.printf "Time: %.6f seconds\n" (end_time -. start_time)