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

module Make (Ord: OrderedType) = struct
  type k = Ord.t
  type 'v t = L | N of (k * 'v) * 'v t * 'v t

  let empty = L

  let is_empty t = t = L

  let rec to_list = function
    | L -> []
    | N (x, l, r) -> to_list l @ [x] @ to_list r

  (* 
  * Max function,.
  * Finds the maximum value in a binary search tree.
  * Fails with an error if the tree is empty. 
  *
  * bst: binary search tree to search through 
  *)
  let rec max (bst: 'v t) =
    match bst with
    | L -> failwith "empty tree"
    | N (x, _, L) -> x
    | N (_, _, r) -> max r

  (* 
  * Insertion function.
  * Inserts a node into the a key-value tree. If the key already exists, the 
  * current value of that key is replaced by the value parameter.
  *
  * bst: binary search tree to insert into
  * key: the key the tuple will have
  * value: the value of the tuple 
  *)
  let rec insert (bst: 'v t) (key: k)(value: 'v) =
    match bst with
    | L -> N ((key, value), L, L)
    | N ((k,v), l, r) when Ord.compare key k < 0 -> 
        N ((k,v), insert l key value, r)
    | N ((k,v), l, r) when Ord.compare key k > 0 -> 
        N ((k,v), l, insert r key value)
    | N ((k,_), l, r) -> N ((k, value), l, r)

  (* 
  * Find function. 
  * Searches within the binary search tree for a key and returns the key's value.
  * Returns 'None' if the key does not exist.
  * 
  * bst: binary search tree to search through
  * key: key to find
  *)
  let rec find (bst: 'v t) (key: k) =
    match bst with
    | L -> None
    | N ((k,_), l, _) when Ord.compare key k < 0 -> find l key   
    | N ((k,_), _, r) when Ord.compare k key < 0 -> find r key
    | N ((_,v), _, _) -> Some v

  (*
  * Delete function.
  * Finds the key and removes its node from the tree, replacing it with the 
  * appropraite node. If the node found has has only has one leaf, replace it 
  * with that leaf.
  * If the node has two leaves, replaces it with the highest value on its left
  * node.
  * If the node doesn't exist the tree is unchanged.
  *
  * bst: binary search tree to delete from
  * key: key to delete
  *)
  let rec delete (bst: 'v t) (key: k) =
    match bst with
    | L -> L
    | N ((k, v), l, r) when Ord.compare key k < 0 -> N ((k,v), delete l key, r)
    | N ((k, v), l, r) when Ord.compare k key < 0 -> N ((k,v), l, delete r key)
    | N (_, L, L) -> L 
    | N (_, L, r) -> r
    | N (_, l, L) -> l
    | N (_, l, r) -> 
        let (k1, v1) = max l in
        N ((k1, v1), delete l k1, r)

  (*
  * Converts a 2-tuple list into a binary search tree
  *
  * lst: list to convert
  *)
  let kvtree_of_list (lst: (k *'v) list) =
  let rec aux lst' acc =
    match lst' with
    | [] -> acc
    | (k,v)::t -> aux t (insert acc k v)
  in aux lst L 
end