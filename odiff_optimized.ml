open Stdlib_upstream_compatible

type pixel = { r : float#; g : float#; b : float#; a : float# }

let white_pixel : pixel = { r = #255.; g = #255.; b = #255.; a = #0. }

let blend_channel_white color alpha =
  Float_u.add #255.0 (Float_u.mul (Float_u.sub color #255.0) alpha)

let blendsemitransparentpixel = function
  | { r; g; b; a } when Float_u.equal a #0.0 -> white_pixel
  | { r; g; b; a } when Float_u.equal a #255. -> { r; g; b; a = #1. }
  | { r; g; b; a } when Float_u.compare a #255. < 0 ->
      let normalizedalpha = Float_u.div a #255. in
      let new_r = blend_channel_white r normalizedalpha in
      let new_g = blend_channel_white g normalizedalpha in
      let new_b = blend_channel_white b normalizedalpha in
      let new_a = normalizedalpha in
      { r = new_r; g = new_g; b = new_b; a = new_a }
  | _ ->
      failwith
        "found pixel with alpha value greater than uint8 max value. aborting."

let decoderawpixel (pixel : int32#) =
  let a =
    Int32_u.logand
      (Int32_u.shift_right_logical pixel 24)
      (Int32_u.of_int32 255l)
  in
  let b =
    Int32_u.logand
      (Int32_u.shift_right_logical pixel 16)
      (Int32_u.of_int32 255l)
  in
  let g =
    Int32_u.logand (Int32_u.shift_right_logical pixel 8) (Int32_u.of_int32 255l)
  in
  let r = Int32_u.logand pixel (Int32_u.of_int32 255l) in

  {
    r = Float_u.of_float (Int32.to_float (Int32_u.to_int32 r));
    g = Float_u.of_float (Int32.to_float (Int32_u.to_int32 g));
    b = Float_u.of_float (Int32.to_float (Int32_u.to_int32 b));
    a = Float_u.of_float (Int32.to_float (Int32_u.to_int32 a));
  }
[@@inline]

let rgb2y { r; g; b; a } =
  Float_u.add
    (Float_u.add (Float_u.mul r #0.29889531) (Float_u.mul g #0.58662247))
    (Float_u.mul b #0.11448223)

let rgb2i { r; g; b; a } =
  Float_u.sub
    (Float_u.sub (Float_u.mul r #0.59597799) (Float_u.mul g #0.27417610))
    (Float_u.mul b #0.32180189)

let rgb2q { r; g; b; a } =
  Float_u.add
    (Float_u.sub (Float_u.mul r #0.21147017) (Float_u.mul g #0.52261711))
    (Float_u.mul b #0.31114694)

let calculatepixelcolordelta pixela pixelb =
  let pixela = pixela |> decoderawpixel |> blendsemitransparentpixel in
  let pixelb = pixelb |> decoderawpixel |> blendsemitransparentpixel in

  let y = Float_u.sub (rgb2y pixela) (rgb2y pixelb) in
  let i = Float_u.sub (rgb2i pixela) (rgb2i pixelb) in
  let q = Float_u.sub (rgb2q pixela) (rgb2q pixelb) in

  let delta =
    Float_u.add
      (Float_u.add
         (Float_u.mul #0.5053 (Float_u.mul y y))
         (Float_u.mul #0.299 (Float_u.mul i i)))
      (Float_u.mul #0.1957 (Float_u.mul q q))
  in
  delta

let calculatepixelbrightnessdelta pixela pixelb =
  let pixela = pixela |> decoderawpixel |> blendsemitransparentpixel in
  let pixelb = pixelb |> decoderawpixel |> blendsemitransparentpixel in
  Float_u.sub (rgb2y pixela) (rgb2y pixelb)

let () =
  let pixela = #0xff33a1cel in
  let pixelb = #0xff33a1cel in
  let delta = calculatepixelcolordelta pixela pixelb in
  Printf.printf "color delta: %f\n" (Float_u.to_float delta)
