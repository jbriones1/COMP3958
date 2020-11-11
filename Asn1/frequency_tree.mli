module type Frequency_tree = sig
  type t
  val compare : t -> t -> int
  val merge : t -> t -> t
end