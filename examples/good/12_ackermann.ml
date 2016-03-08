let rec ackermann = function m -> function n ->
  let n_zero = function
      true -> ackermann (m - 1) 1
    | false -> ackermann (m - 1) (ackermann m (n - 1))
  in
  let m_zero = function
      true -> n + 1
    | false -> n_zero (n = 0)
  in
  m_zero (m = 0)

let res = ackermann 2 4
