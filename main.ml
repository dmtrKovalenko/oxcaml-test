type 'a option_custom = Null | This of 'a

let hd = function [] -> Null | x :: _ -> This x
let rec tl = function [] -> Null | [ x ] -> This x | _ :: xs -> tl xs

let add_arrays_imperative a b c =
  let n = Array.length a in
  for i = 0 to n - 1 do
    c.(i) <- a.(i) +. b.(i)
  done

(* SIMD array addition using Float64x2 *)
let add_arrays_simd a b c =
  let n = Array.length a in
  let simd_size = 2 in
  let i = ref 0 in

  while !i + simd_size <= n do
    let va = Ocaml_simd_sse.Float64x2.set a.(!i) a.(!i + 1) in
    let vb = Ocaml_simd_sse.Float64x2.set b.(!i) b.(!i + 1) in
    let vc = Ocaml_simd_sse.Float64x2.add va vb in
    c.(!i) <- Ocaml_simd_sse.Float64x2.extract ~idx:0 vc;
    c.(!i + 1) <- Ocaml_simd_sse.Float64x2.extract ~idx:1 vc;
    i := !i + simd_size
  done;

  for j = !i to n - 1 do
    c.(j) <- a.(j) +. b.(j)
  done

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
  let sum_simd = Array.create_float (Array.length a_simd) in

  add_arrays_simd a_simd b_simd sum_simd;
  Printf.printf "SIMD result: ";
  Array.iter (Printf.printf "%f ") sum_simd;
  Printf.printf "\n";

  Printf.printf "SIMD operations completed\n"
