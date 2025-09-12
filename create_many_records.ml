type floatrecord = { x : float; y : float; z : float }

let pure_stack_float n =

let ()  = pure_stack_float 1.0 |> Printf.printf "Result: %f\n"
