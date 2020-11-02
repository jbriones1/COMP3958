type 'a leftist = E | T of int * 'a * 'a leftist * 'a leftist

let rank = function
  | E -> 0
  | T (r, _, _, _) -> r

let value = function
  | E -> 0
  | T (_, v, _, _) -> v

let empty = E

let rec merge t1 t2 =
  match t1, t2 with
  | E, t | t, E -> t
  | T (_, x1, _, _), T(_, x2, _, _)  when x2 < x1 -> merge t2 t1
  | T (_, x, l, r), t ->
    let r' = merge r t in
    if rank l >= rank r' then T (1 + rank r', x, l, r')
    else T (1 + rank l, x, r', l)

let insert t x = merge t (T (1, x, E, E))

let heap_of_list l =
  let open Base in
  List.fold_left ~init:E ~f:(fun t x -> insert t x) l

let get_min = function
  | E -> None
  | T (_, x, _, _) -> Some x

let delete_min = function
  | E -> failwith "delete_min: empty tree"
  | T (_, _, l, r) -> merge l r

let rec is_min_heap = function
  | E -> true
  | T (_, _, E, E) -> true
  | T (_, x, l, E) ->
    if x > value l then false
    else is_min_heap l
  | T (_, x, E, r) ->
    if x > value r then false
    else is_min_heap r
  | T (_, x, l, r) -> 
    if x > value l || x > value r then false
    else is_min_heap l && is_min_heap r
    
let a = T (2, 2, T (2, 6, T (1, 7, E, E), T (1, 8, E, E)), T (1, 3, E, E))
let b = T (2, 6, T (2, 2, T (1, 7, E, E), T (1, 8, E, E)), T (1, 3, E, E))

let test_empty_heap = is_min_heap E
let test_correct_heap = is_min_heap a
let test_incorrect_heap = is_min_heap b