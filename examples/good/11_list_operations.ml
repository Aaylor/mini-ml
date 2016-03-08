let rec map = function f -> function
    [] -> []
  | x :: xs -> (f x) :: map f xs

let rec filter = function f -> function
    [] -> []
  | x :: xs ->
    let next = function
        true -> x :: filter f xs
      | false -> filter f xs
    in
    next (f x)

let rev = function l ->
  let rec rev_aux = function acc -> function
      [] -> acc
    | x :: xs -> rev_aux (x :: acc) xs
  in
  rev_aux [] l

let rec fold_left = function f -> function acc -> function
    [] -> acc
  | x :: xs -> fold_left f (f acc x) xs
                 
let succ = function x -> x + 1
let res1 = map succ (1 :: 2 :: 3 :: [])

let less5 = function x -> x < 5
let res2 = filter less5 (1 :: 6 :: 2 :: 7 :: [])

let res3 = rev (1 :: 2 :: 3 :: [])

let mul = function x -> function y -> x * y
let res4 = fold_left mul 1 (1 :: 2 :: 3 :: [])
