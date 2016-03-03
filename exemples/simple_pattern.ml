let f =
  let add = function (x, y) -> x + y | 1 -> 2 | x -> true in
  add (true, 22)
