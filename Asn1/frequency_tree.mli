module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type S = sig
  type elt
  type t
  val empty : t
  val merge : t -> t -> t
  val insert : t -> elt * int -> t
  val heap_of_list : elt list -> t
  val get_min : t -> elt option
  val delete_min : t -> t
  val heap_sort : elt list -> elt list
end

module Make(Ord: OrderedType): S with type elt := Ord.t