type foofib = { foo : int; bar : int }

let check_element foo = if foo.bar > 0 then true else false
let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

let () =
  let n = 35 in
  let x2 = stack_ { foo = n; bar = n } in
  let result = fib n in
  check_element x2 |> ignore
