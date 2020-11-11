module type Frequency_tree = sig
  type t
  val compare : t -> t -> int
  val merge : t -> t -> t
end

module Frequency_tree = struct
  type t = E | T of (string * int) * t * t

  let compare t1 t2 =
    match t1, t2 with
      E, E -> 0
    |  E, (T(_,_,_)) | (T(_,_,_)), E -> 0
    | (T((_, x1), _, _)), (T((_, x2), _, _)) -> Base.Int.compare x1 x2

  let merge t1 t2 =
    match t1, t2 with
      E, E -> E
    | t, E | E, t -> t
    | (T ((_, v1), _, _) as tl), (T ((_, v2), _, _) as tr) -> 
      T(("-", v1 + v2), tl, tr)
end

module H = Leftist_heap.Make(Frequency_tree)

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


let ftree_of_list lst =
  let rec aux lst' acc =
    match lst' with
    | [] -> acc
    | x::t -> aux t (insert acc x)
  in aux lst E

let rec to_list = function
    E -> []
  | T (x, l, r) -> to_list l @ [x] @ to_list r

(******************************************************************************)
let rec assign_codes code = function
    E -> []
  | T((s, _), E, E) -> [(s, code)]
  | T(_, l, r) -> assign_codes (code ^ "0") l
                  @ assign_codes (code ^ "1") r
let rec huffman_tree hp =
  match H.to_list hp with 
    [] -> E
  | [x] -> x
  | x::y::t ->
    let next_list = H.heap_of_list t in
    huffman_tree (H.insert next_list (merge x y))

let huffman_of_list lst =
  assign_codes "" (huffman_tree @@ H.heap_of_list @@ nodelist_of_tuplist lst)

let text = [("C", 2);("B", 6);("E", 7);("_", 10);("D", 10);("A", 11)]

let test = huffman_of_list text

