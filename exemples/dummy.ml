let result =
  let f = function g -> function [] -> [] | x :: xs -> (g x) :: xs in
  let g = function x -> x + 1 in
  f g

