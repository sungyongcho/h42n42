type intlist =
  | Nil
  | Cons of int * intlist


let rec length = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t

