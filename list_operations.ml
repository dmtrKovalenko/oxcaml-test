open Stdlib_upstream_compatible
open Ocaml_simd

type 'a option_custom = Null | This of 'a

let hd = function [] -> Null | x :: _ -> This x
let rec tl = function [] -> Null | [ x ] -> This x | _ :: xs -> tl xs

let add_arrays_imperative a b c =
  let n = Array.length a in
  for i = 0 to n - 1 do
    c.(i) <- a.(i) +. b.(i)
  done

module Float64x2 = Ocaml_simd.Float64x2
module Float64x4 = Ocaml_simd.Float64x4
module Int8x16 = Ocaml_simd.Int8x16
module Int64x2 = Ocaml_simd.Int64x2

let add_arrays_simd_f64x2 a b c =
  let n = Array.length a in
  let simd_size = 2 in
  let i = ref 0 in
  
  while !i + simd_size <= n do
    let va = Float64x2.Float_array.get a ~idx:!i in
    let vb = Float64x2.Float_array.get b ~idx:!i in
    let vc = Float64x2.add va vb in
    Float64x2.Float_array.set c ~idx:!i vc;
    i := !i + simd_size
  done;
  
  for j = !i to n - 1 do
    c.(j) <- a.(j) +. b.(j)
  done

let add_arrays_simd_f64x4 a b c =
  let n = Array.length a in
  let simd_size = 4 in
  let i = ref 0 in
  
  while !i + simd_size <= n do
    let va = Float64x4.Float_array.get a ~idx:!i in
    let vb = Float64x4.Float_array.get b ~idx:!i in
    let vc = Float64x4.add va vb in
    Float64x4.Float_array.set c ~idx:!i vc;
    i := !i + simd_size
  done;
  
  for j = !i to n - 1 do
    c.(j) <- a.(j) +. b.(j)
  done

let lol () =
  let a = [: 1.0; 2.0; 3.0 :] in
  let b = [: 4.0; 5.0; 6.0 :] in
  let sum = Array.create_float 3 in
  ()

let () =
  let list = [ 1; 2; 3; 4; 5 ] in
  let h = hd list in
  let t = tl list in

  Printf.printf "hd: %s; tl: %s\n"
    (match h with Null -> "Null" | This v -> string_of_int v)
    (match t with Null -> "Null" | This v -> string_of_int v);

  let a = [| 1.0; 2.0; 3.0 |] in
  let b = [| 4.0; 5.0; 6.0 |] in
  let sum = Array.create_float 3 in
  add_arrays_imperative a b sum;
  Array.iter (Printf.printf "%f ") sum;
  Printf.printf "\n";

  let a_simd = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |] in
  let b_simd = [| 8.0; 7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0 |] in
  let sum_simd2 = Array.create_float (Array.length a_simd) in
  let sum_simd4 = Array.create_float (Array.length a_simd) in
  
  add_arrays_simd_f64x2 a_simd b_simd sum_simd2;
  Printf.printf "SIMD Float64x2 result: ";
  Array.iter (Printf.printf "%f ") sum_simd2;
  Printf.printf "\n";
  
  add_arrays_simd_f64x4 a_simd b_simd sum_simd4;
  Printf.printf "SIMD Float64x4 result: ";
  Array.iter (Printf.printf "%f ") sum_simd4;
  Printf.printf "\n";

  (* Example SIMD operations like in OxCaml docs *)
  let text = "abcdefghijklmnopqrstuvwxyz" in
  let floats = [| 1.0; 2.0 |] in
  let ints = [| 1; 2 |] in
  
  let _ = Int8x16.String.get text ~byte:0 in
  let _ = Float64x2.Float_array.get floats ~idx:0 in
  let _ = Int64x2.Immediate_array.get_tagged ints ~idx:0 in
  
  Printf.printf "SIMD string/array access examples completed\n";

