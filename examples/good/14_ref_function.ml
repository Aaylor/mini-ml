let f = ref (function x -> x)
let change = f := (function x -> x + 1)
let res = (!f) 41
