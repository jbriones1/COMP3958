module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type S = sig
  type elt
  type t
  val empty : t
  val merge : t -> t -> t
  val insert : t -> elt -> t
  val heap_of_list : elt list -> t
  val get_min : t -> elt option
  val delete_min : t -> t
  val heap_sort : elt list -> elt list
  val to_list : t -> elt list
end

module Make(Ord: OrderedType) = struct
  type elt = Ord.t
  type t = E | T of int * elt * t * t

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r

  let empty = E

  let rec merge t1 t2 =
    match t1, t2 with
    | E, t | t, E -> t
    | T (_, x1, _, _), T(_, x2, _, _)  when Ord.compare x2 x1 < 0 ->
      merge t2 t1
    | T (_, x, l, r), t ->
      let r' = merge r t in
      if rank l >= rank r' then T (1 + rank r', x, l, r')
      else T (1 + rank l, x, r', l)

  let insert t x = merge t (T (1, x, E, E))

  let heap_of_list l =
    let open Base in
    List.fold_left ~init:E ~f:(fun h x -> insert h x) l

  let get_min = function
    | E -> None
    | T (_, x, _, _) -> Some x

  let delete_min = function
    | E -> failwith "delete_min: empty tree"
    | T (_, _, l, r) -> merge l r

  let heap_sort lst =
    let h_list = heap_of_list lst in
    let rec aux lst' heap =
      match heap with
      | E -> List.rev lst'
      | T (_, x, _, _) ->  aux (x::lst') (delete_min heap) in
    aux [] h_list

  let to_list h =
    let rec aux acc h' =
      match h' with
        E -> List.rev acc
      | T (_, x, _, _) -> aux (x::acc) (delete_min h') in
    aux [] h
end

