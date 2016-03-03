let result =
  let a = ref (function x -> x) in
  let c = (!a) 10 in
  let _ = a := (function x -> x + 1) in
  a
