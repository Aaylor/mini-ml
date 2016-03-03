let result =
  let rec f = function g -> function
      [] -> []
    | x :: xs -> g x :: f g xs
  in f
