module Frequency_tree = struct
  type t = E | T of (string * int) * t * t

  let compare t1 t2 =
    match t1, t2 with
      E, E -> 0
    |  E, (T(_,_,_)) | (T(_,_,_)), E -> 0
    | (T((s1, x1), _, _)), (T((s2, x2), _, _)) -> 
      let v = Base.Int.compare x1 x2 in
      if v <> 0 then v
      else String.compare s2 s1
end

(* The heap consisting of Frequency trees *)
module H = Leftist_heap.Make(Frequency_tree)

include Frequency_tree

(* 
 * Merges two frequency tree nodes into one, by creating a parent node with the
 * sum of the nodes' frequencies. Sets the parent node's char to nothing.
 *
 * t1: left node to merge
 * t2: right node to merge
 *)
let merge t1 t2 =
  match t1, t2 with
    E, E -> E
  | t, E | E, t -> t
  | (T ((_, v1), _, _) as tl), (T ((_, v2), _, _) as tr) -> 
    T(("", v1 + v2), tl, tr)

(*
 * Creates a list of frequency tree nodes from the a list of frequency tuples.
 * 
 * lst: lst of frequency tuples
 *)
let nodelist_of_tuplist lst =
  Base.List.map ~f: (fun x -> T(x, E, E)) lst

(******************************************************************************)
(* 
 * Finds the leaves in a frequency tree and assigns the Huffman codes to them.
 * Moving left adds a "0" to the code, moving right adds a "1" to the code.
 * 
 * code: the code to assign to the character
 *)
let rec assign_codes code = function
    E -> []
  | T((s, _), E, E) -> [(s, code)]
  | T(_, l, r) -> assign_codes (code ^ "0") l
                  @ assign_codes (code ^ "1") r

(* 
 * Creates the Huffman tree from a frequency tree heap.
 * Takes the two smallest nodes in the heap and merges them, reinserting it back
 * into the list. Continues this until there is only one node left in the heap
 * and returns the node, which should be the completed Huffman tree.
 *
 * hp: the heap of Frequency trees
 *)
let rec huffman_tree hp =
  match H.to_list hp with 
    [] -> E
  | [x] -> x
  | x::y::t ->
    let next_list = H.heap_of_list t in
    huffman_tree (H.insert next_list (merge x y))

(*
 * Gets the Huffman codes list from the frequency list.
 * 
 * lst: the list of character frequencies 
 *)
let huffman_of_list lst =
  assign_codes "" (huffman_tree @@ H.heap_of_list @@ nodelist_of_tuplist lst)

