let x =
  match not true with
  | true -> "nope"
  | false -> "yep"

  let y =
    match 42 with
    | fooo -> fooo

let z =
  match "foo" with
  | "bar" -> 0
  | _ -> 1

let a =
  match [1;2] with
  | [] -> "empty"
  | _ -> "not empty"

let b =
  match ["taylor";"swift"] with
  | [] -> ["folklore"]
  | h :: t -> t

  let fst3 t =
    match t with
    | (a, b, c) -> a
(* ============== *)
let empty lst =
  match lst with
  | [] -> true
  | _ -> false


let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t

let rec length lst =
  match lst with
  | [] -> 0
  | h :: t -> 1 + length t

(* example usage:
  append [1;2;3] [4;5;6] is [1;2;3;4;5] *)

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: (append t lst2)

(* Error: The function append_mine has type 'a list * 'a list -> 'a list
       It is applied to too many arguments
Line 1, characters 18-25:
  This extra argument is not expected.
let rec append_mine = function
  | ([], lst2) -> lst2
  | (h :: t, lst2) -> h :: (append_mine (t, lst2)) *)

