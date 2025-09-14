open Stdlib_upstream_compatible

type 'a option_custom = Null | This of 'a

let hd = function [] -> Null | x :: _ -> This x
let rec tl = function [] -> Null | [ x ] -> This x | _ :: xs -> tl xs

let add_arrays_imperative a b c =
  let n = Iarray.length a in
  for i = 0 to n - 1 do
    c.(i) <- a.(i) +. b.(i)
  done

let lol () =
  let a = [: 1.0; 2.0; 3.0 :] in
  let b = [: 4.0; 5.0; 6.0 :] in
  let sum = Array.create_float (Array.length a) in

let () =
  let list = [ 1; 2; 3; 4; 5 ] in
  let h = hd list in
  let t = tl list in

  Printf.printf "hd: %s; tl: %s\n"
    (match h with Null -> "Null" | This v -> string_of_int v)
    (match t with Null -> "Null" | This v -> string_of_int v);

  let a = [: 1.0; 2.0; 3.0 :] in
  let b = [: 4.0; 5.0; 6.0 :] in
  let sum = Array.create_float (Array.length a) in
  add_arrays_imperative a b sum;
  Array.iter (Printf.printf "%f ") sum;

