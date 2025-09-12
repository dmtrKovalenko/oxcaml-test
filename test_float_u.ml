open Stdlib_upstream_compatible

let test_float_u () =
  let x = #1.0 in
  let y = #2.0 in
  Float_u.equal x y

