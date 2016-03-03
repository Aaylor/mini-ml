let value =
  let l = 1 :: 2 :: 3 :: [] in
  let rec map = function f -> function [] -> [] | x :: l -> f x :: map f l in
  let func = (function x -> x + 1) in
  let fmap = map func in
  fmap l
