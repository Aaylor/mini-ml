let result =
  let f = ref in
  let r = f (function x -> x) in
  let d = r := (function x -> x + 1) in
  (!r) true
