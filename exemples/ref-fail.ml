let result =
  let r = ref (function x -> x) in
  let d = r := (function x -> x + 1) in
  (!r) true
