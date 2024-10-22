(* requires: [n >= 0]*)
let rec fact n =
  if n = 0 then 1
  else n * fact (n - 1)
  (*           not  fact n - 1 *)
