type t

val empty : t
val add : string -> t -> t
val find : string -> t -> bool
val count : t -> int
val of_list : string list -> t