module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type S = sig
  type k
  type 'v t

  val empty : 'v t
  val is_empty : 'v t -> bool
  val to_list : 'v t -> (k * 'v) list
  val max : 'v t -> (k * 'v)
  val insert : 'v t -> k -> 'v -> 'v t
  val find : 'v t -> k -> 'v option
  val delete : 'v t -> k -> 'v t
  val kvtree_of_list : (k * 'v) list -> 'v t
end

module Make (Ord: OrderedType): S with type k := Ord.t