
let main =
  let l =  1 :: 2 :: 3 :: 4 :: [] in
  let fmap = function f -> function
      [] -> []
    | x :: xs -> f x :: xs
  in fmap (function x -> x + 1) l
