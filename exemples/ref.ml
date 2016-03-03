let result =
  let x = ref 42 in
  let _ = x := 41 in
  !x
