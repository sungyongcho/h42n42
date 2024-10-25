module MyStack = struct
  type 'a stack = Empty | Entry of 'a * 'a stack

  let empty = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end

module ListStack = struct
  type 'a stack = 'a list

  let empty = []
  let push x s = x :: s
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end
