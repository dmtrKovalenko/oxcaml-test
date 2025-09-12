open Int32

(* Performance-optimized version using standard OCaml types
   but structured for OxCaml's advanced optimizations *)

type pixel = { r : float; g : float; b : float; a : float }

let white_pixel : pixel = { r = 255.; g = 255.; b = 255.; a = 0. }

(* Precomputed constants for better optimization *)
let y_r_coeff = 0.29889531
let y_g_coeff = 0.58662247  
let y_b_coeff = 0.11448223
let i_r_coeff = 0.59597799
let i_g_coeff = -0.27417610
let i_b_coeff = -0.32180189
let q_r_coeff = 0.21147017
let q_g_coeff = -0.52261711
let q_b_coeff = 0.31114694

(* Aggressive inlining for hot functions *)
let blend_channel_white color alpha = 255. +. ((color -. 255.) *. alpha)
[@@inline always] [@@specialise always]

let blendSemiTransparentPixel = function
  | { r; g; b; a } when a = 0. -> white_pixel
  | { r; g; b; a } when a = 255. -> { r; g; b; a = 1. }
  | { r; g; b; a } when a < 255. ->
      let normalizedAlpha = a /. 255. in
      let r = blend_channel_white r normalizedAlpha in
      let g = blend_channel_white g normalizedAlpha in
      let b = blend_channel_white b normalizedAlpha in
      { r; g; b; a = normalizedAlpha }
  | _ ->
      failwith "Found pixel with alpha value greater than uint8 max value. Aborting."
[@@inline always] [@@specialise always]

let decodeRawPixel pixel =
  let a = logand (shift_right_logical pixel 24) 255l in
  let b = logand (shift_right_logical pixel 16) 255l in
  let g = logand (shift_right_logical pixel 8) 255l in
  let r = logand pixel 255l in
  {
    r = Int32.to_float r;
    g = Int32.to_float g;
    b = Int32.to_float b;
    a = Int32.to_float a;
  }
[@@inline always] [@@specialise always]

(* Color space conversions optimized for FMA *)
let rgb2y { r; g; b; a = _ } =
  (r *. y_r_coeff) +. (g *. y_g_coeff) +. (b *. y_b_coeff)
[@@inline always] [@@specialise always]

let rgb2i { r; g; b; a = _ } =
  (r *. i_r_coeff) +. (g *. i_g_coeff) +. (b *. i_b_coeff)
[@@inline always] [@@specialise always]

let rgb2q { r; g; b; a = _ } =
  (r *. q_r_coeff) +. (g *. q_g_coeff) +. (b *. q_b_coeff)
[@@inline always] [@@specialise always]

(* Main calculation structured for vectorization *)
let calculatePixelColorDelta pixelA pixelB =
  let pixelA = pixelA |> decodeRawPixel |> blendSemiTransparentPixel in
  let pixelB = pixelB |> decodeRawPixel |> blendSemiTransparentPixel in

  (* Compute all color space coordinates *)
  let y_a = rgb2y pixelA in
  let i_a = rgb2i pixelA in  
  let q_a = rgb2q pixelA in
  let y_b = rgb2y pixelB in
  let i_b = rgb2i pixelB in
  let q_b = rgb2q pixelB in

  (* Compute differences *)
  let y_diff = y_a -. y_b in
  let i_diff = i_a -. i_b in
  let q_diff = q_a -. q_b in

  (* Weighted sum of squares - pattern that vectorizes well *)
  (0.5053 *. y_diff *. y_diff) +. (0.299 *. i_diff *. i_diff) +. (0.1957 *. q_diff *. q_diff)
[@@inline always] [@@specialise always]

let calculatePixelBrightnessDelta pixelA pixelB =
  let pixelA = pixelA |> decodeRawPixel |> blendSemiTransparentPixel in
  let pixelB = pixelB |> decodeRawPixel |> blendSemiTransparentPixel in
  rgb2y pixelA -. rgb2y pixelB
[@@inline always] [@@specialise always]

(* Vectorization-friendly batch processing with loop unrolling *)
let calculateBatchColorDeltas pixelsA pixelsB =
  let len = Array.length pixelsA in
  let results = Array.create_float len in
  
  (* Unroll by 8 for better SIMD utilization *)
  let rec process_batch i =
    if i + 7 < len then (
      (* Process 8 pixels at once - optimal for 256-bit SIMD *)
      results.(i) <- calculatePixelColorDelta pixelsA.(i) pixelsB.(i);
      results.(i+1) <- calculatePixelColorDelta pixelsA.(i+1) pixelsB.(i+1);
      results.(i+2) <- calculatePixelColorDelta pixelsA.(i+2) pixelsB.(i+2);
      results.(i+3) <- calculatePixelColorDelta pixelsA.(i+3) pixelsB.(i+3);
      results.(i+4) <- calculatePixelColorDelta pixelsA.(i+4) pixelsB.(i+4);
      results.(i+5) <- calculatePixelColorDelta pixelsA.(i+5) pixelsB.(i+5);
      results.(i+6) <- calculatePixelColorDelta pixelsA.(i+6) pixelsB.(i+6);
      results.(i+7) <- calculatePixelColorDelta pixelsA.(i+7) pixelsB.(i+7);
      process_batch (i + 8)
    ) else (
      (* Handle remaining pixels *)
      for j = i to len - 1 do
        results.(j) <- calculatePixelColorDelta pixelsA.(j) pixelsB.(j)
      done
    )
  in
  process_batch 0;
  results

(* Accumulation using Kahan summation for better precision *)
let calculateImageDifference imageA imageB =
  let len = Array.length imageA in
  assert (len = Array.length imageB);
  
  let sum = ref 0.0 in
  let c = ref 0.0 in  (* Compensation for lost low-order bits *)
  
  for i = 0 to len - 1 do
    let delta = calculatePixelColorDelta imageA.(i) imageB.(i) in
    let y = delta -. !c in
    let t = !sum +. y in
    c := (t -. !sum) -. y;
    sum := t
  done;
  
  !sum /. Float.of_int len

(* Enhanced benchmark with performance metrics *)
let run_benchmark () =
  let pixelA = 0xFF33A1CEl in
  let pixelB = 0xFF33A1CFl in  (* Slightly different for non-zero delta *)
  
  Printf.printf "=== OxCaml High-Performance Image Difference ===\n";
  
  (* Single pixel test *)
  let delta = calculatePixelColorDelta pixelA pixelB in
  Printf.printf "Single pixel delta: %f\n" delta;
  
  (* Multi-size benchmarks *)
  let test_sizes = [1000; 10000; 100000; 1000000] in
  
  List.iter (fun test_size ->
    let imageA = Array.make test_size pixelA in
    let imageB = Array.make test_size pixelB in
    
    Printf.printf "\nProcessing %d pixels:\n" test_size;
    
    (* Batch processing benchmark *)
    let start_time = Sys.time () in
    let batch_results = calculateBatchColorDeltas imageA imageB in
    let end_time = Sys.time () in
    
    let processing_time = end_time -. start_time in
    let pixels_per_second = Float.of_int test_size /. processing_time in
    
    Printf.printf "  Batch time: %.6f seconds\n" processing_time;
    Printf.printf "  Throughput: %.0f pixels/second\n" pixels_per_second;
    Printf.printf "  Performance: %.2f MPix/s\n" (pixels_per_second /. 1_000_000.0);
    
    (* Image difference test *)
    let start_time2 = Sys.time () in
    let img_diff = calculateImageDifference imageA imageB in
    let end_time2 = Sys.time () in
    
    Printf.printf "  Image diff time: %.6f seconds\n" (end_time2 -. start_time2);
    Printf.printf "  Average delta: %f\n" 
      (Array.fold_left (+.) 0.0 batch_results /. Float.of_int test_size);
    Printf.printf "  Image difference: %f\n" img_diff;
  ) test_sizes

let () = run_benchmark ()
