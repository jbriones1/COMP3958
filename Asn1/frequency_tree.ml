module Frequency_tree = struct
  type t = E | T of (string * int) * t * t

  let compare t1 t2 =
    match t1, t2 with
      E, E -> 0
    |  E, (T(_,_,_)) | (T(_,_,_)), E -> 0
    | (T((_, x1), _, _)), (T((_, x2), _, _)) -> Base.Int.compare x1 x2
end

module Heap = Leftist_heap.Make(Frequency_tree)

include Frequency_tree

let empty = E

let nodelist_of_tuplist lst =
  Base.List.map ~f: (fun x -> T(x, E, E)) lst

let rec insert t (s, v) =
  match t with
    E -> T ((s, v), E, E)
  | T ((_, v') as n, l, r) when v < v' -> T (n, insert l (s, v), r)
  | T ((_, v') as n, l, r) when v >= v' -> T (n, l, insert r (s, v))
  | _ -> t
let merge t1 t2 =
  match t1, t2 with
    E, E -> E
  | t, E | E, t -> t
  | (T ((_, v1), _, _) as tl), (T ((_, v2), _, _) as tr) -> 
    T(("-", v1 + v2), tl, tr)

let get_root_freq = function
    E -> failwith "Empty tree"
  | T((_, v), _, _) -> v

let ftree_of_list lst =
  let rec aux lst' acc =
    match lst' with
    | [] -> acc
    | x::t -> aux t (insert acc x)
  in aux lst E

let rec to_list = function
    E -> []
  | T (x, l, r) -> to_list l @ [x] @ to_list r

