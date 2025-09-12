open Int32

(* Working OxCaml optimized version - step by step approach *)

(* Use unboxed record type *)
type pixel = #{ r : float#; g : float#; b : float#; a : float# }

(* Regular float constants for now - will work on unboxed conversion *)
let y_r_coeff = 0.29889531
let y_g_coeff = 0.58662247  
let y_b_coeff = 0.11448223
let i_r_coeff = 0.59597799
let i_g_coeff = -0.27417610
let i_b_coeff = -0.32180189
let q_r_coeff = 0.21147017
let q_g_coeff = -0.52261711
let q_b_coeff = 0.31114694

(* Helper function to create unboxed pixel from regular floats *)
let make_pixel r g b a : pixel =
  #{ r; g; b; a }

let blend_channel_white color alpha = 255.0 +. ((color -. 255.0) *. alpha)
[@@inline always]

let blendSemiTransparentPixel (pixel : pixel) : pixel =
  let #{ r; g; b; a } = pixel in
  
  (* Convert to regular floats for comparison *)
  if a = 0.0 then 
    make_pixel 255.0 255.0 255.0 0.0  
  else if a = 255.0 then 
    #{ r; g; b; a = 1.0 }
  else if a < 255.0 then
    let normalizedAlpha = a /. 255.0 in
    #{ r; g; b; a = normalizedAlpha }
  else
    failwith "Found pixel with alpha value greater than uint8 max value. Aborting."
[@@inline always]

let decodeRawPixel pixel =
  let a = logand (shift_right_logical pixel 24) 255l in
  let b = logand (shift_right_logical pixel 16) 255l in
  let g = logand (shift_right_logical pixel 8) 255l in
  let r = logand pixel 255l in
  make_pixel 
    (Int32.to_float r)
    (Int32.to_float g) 
    (Int32.to_float b)
    (Int32.to_float a)
[@@inline always]

(* Color space conversions - return regular floats for now *)
let rgb2y (#{ r; g; b; a = _ } : pixel) : float =
  (r *. y_r_coeff) +. (g *. y_g_coeff) +. (b *. y_b_coeff)
[@@inline always]

let rgb2i (#{ r; g; b; a = _ } : pixel) : float =
  (r *. i_r_coeff) +. (g *. i_g_coeff) +. (b *. i_b_coeff)  
[@@inline always]

let rgb2q (#{ r; g; b; a = _ } : pixel) : float =
  (r *. q_r_coeff) +. (g *. q_g_coeff) +. (b *. q_b_coeff)
[@@inline always]

(* Core optimized calculation *)
let calculatePixelColorDelta pixelA pixelB =
  let pixelA = pixelA |> decodeRawPixel |> blendSemiTransparentPixel in
  let pixelB = pixelB |> decodeRawPixel |> blendSemiTransparentPixel in

  let y_a = rgb2y pixelA in
  let i_a = rgb2i pixelA in  
  let q_a = rgb2q pixelA in
  let y_b = rgb2y pixelB in
  let i_b = rgb2i pixelB in
  let q_b = rgb2q pixelB in

  let y_diff = y_a -. y_b in
  let i_diff = i_a -. i_b in
  let q_diff = q_a -. q_b in

  (* Delta calculation *)
  (0.5053 *. y_diff *. y_diff) +. (0.299 *. i_diff *. i_diff) +. (0.1957 *. q_diff *. q_diff)
[@@inline always]

let calculatePixelBrightnessDelta pixelA pixelB =
  let pixelA = pixelA |> decodeRawPixel |> blendSemiTransparentPixel in
  let pixelB = pixelB |> decodeRawPixel |> blendSemiTransparentPixel in
  let y_a = rgb2y pixelA in 
  let y_b = rgb2y pixelB in
  y_a -. y_b
[@@inline always]

(* Vectorization-friendly batch processing *)
let calculateBatchColorDeltas pixelsA pixelsB =
  let len = Array.length pixelsA in
  let results = Array.create_float len in
  
  (* Unroll by 4 to help the vectorizer *)
  let rec process_batch i =
    if i + 3 < len then (
      results.(i) <- calculatePixelColorDelta pixelsA.(i) pixelsB.(i);
      results.(i+1) <- calculatePixelColorDelta pixelsA.(i+1) pixelsB.(i+1);
      results.(i+2) <- calculatePixelColorDelta pixelsA.(i+2) pixelsB.(i+2);
      results.(i+3) <- calculatePixelColorDelta pixelsA.(i+3) pixelsB.(i+3);
      process_batch (i + 4)
    ) else (
      for j = i to len - 1 do
        results.(j) <- calculatePixelColorDelta pixelsA.(j) pixelsB.(j)
      done
    )
  in
  process_batch 0;
  results

(* Optimized image difference calculation *) 
let calculateImageDifference imageA imageB =
  let len = Array.length imageA in
  assert (len = Array.length imageB);
  
  let total_diff = ref 0.0 in
  
  (* Simple vectorizable loop *)
  for i = 0 to len - 1 do
    let delta = calculatePixelColorDelta imageA.(i) imageB.(i) in
    total_diff := !total_diff +. delta
  done;
  
  !total_diff /. Float.of_int len

(* Benchmark function *)
let run_benchmark () =
  let pixelA = 0xFF33A1CEl in
  let pixelB = 0xFF33A1CEl in
  
  Printf.printf "=== OxCaml Advanced Optimized Image Difference ===\n";
  
  (* Single pixel test *)
  let delta = calculatePixelColorDelta pixelA pixelB in
  Printf.printf "Single pixel delta: %f\n" delta;
  
  (* Batch processing benchmark *)
  let test_size = 1000000 in
  let imageA = Array.make test_size pixelA in
  let imageB = Array.make test_size pixelB in
  
  Printf.printf "Processing %d pixels with unboxed types...\n" test_size;
  
  let start_time = Sys.time () in
  let batch_results = calculateBatchColorDeltas imageA imageB in
  let end_time = Sys.time () in
  
  let processing_time = end_time -. start_time in
  let pixels_per_second = Float.of_int test_size /. processing_time in
  
  Printf.printf "Time: %.6f seconds\n" processing_time;
  Printf.printf "Throughput: %.0f pixels/second\n" pixels_per_second;
  Printf.printf "Average delta: %f\n" 
    (Array.fold_left (+.) 0.0 batch_results /. Float.of_int test_size);
  
  (* Image difference test *)
  let img_diff = calculateImageDifference imageA imageB in
  Printf.printf "Image difference: %f\n" img_diff

let () = run_benchmark ()