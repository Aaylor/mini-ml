let x =
  let length = function [] -> 0 | x::xs -> x + 1 in
  length (true :: [])
