let res =
  let f = ref (function x -> x) in
  f := (function x -> x + 1);
  (!f) true
    
