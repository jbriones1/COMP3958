open Base
open Stdio

type ('k, 'v) bstree = L | N of ('k * 'v) * ('k, 'v) bstree * ('k, 'v) bstree

(* 
 * Max function,.
 * Finds the maximum value in a binary search tree.
 * Fails with an error if the tree is empty. 
 *
 * bst: binary search tree to search through 
 *)
let rec max (bst: ('k, 'v) bstree) =
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
 * compare: function to compare the keys
 * key: the key the tuple will have
 * value: the value of the tuple 
 *)
let rec insert (bst: ('k, 'v) bstree) ~compare:f (key: 'k) (value: 'v) =
  match bst with
  | L -> N ((key, value), L, L)
  | N ((k,v), l, r) when f key k < 0 -> 
      N ((k,v), insert l ~compare:f key value, r)
  | N ((k,v), l, r) when f key k > 0 -> 
      N ((k,v), l, insert r ~compare:f key value)
  | N ((k,_), l, r) -> N ((k, value), l, r)

(* 
 * Find function. 
 * Searches within the binary search tree for a key and returns the key's value.
 * Returns 'None' if the key does not exist.
 * 
 * bst: binary search tree to search through
 * compare: function to compare keys
 * key: key to find
 *)
let rec find (bst: ('k, 'v) bstree) ~compare:f key =
  match bst with
  | L -> None
  | N ((k,_), l, _) when f key k < 0 -> find l ~compare:f key   
  | N ((k,_), _, r) when f k key < 0 -> find r ~compare:f key
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
 * compare: function to compare keys
 * key: key to delete
 *)
let rec delete (bst: ('k, 'v) bstree) ~compare:f (key: 'k) =
  match bst with
  | L -> L
  | N ((k, v), l, r) when f key k < 0 -> N ((k,v), delete l ~compare:f key, r)
  | N ((k, v), l, r) when f k key < 0 -> N ((k,v), l, delete r ~compare:f key)
  | N (_, L, L) -> L 
  | N (_, L, r) -> r
  | N (_, l, L) -> l
  | N (_, l, r) -> 
      let (k1, v1) = max l in
      N ((k1, v1), delete l ~compare:f k1, r)

(*
 * Converts a 2-tuple list into a binary search tree
 *
 * lst: list to convert
 * compare: function to compare the keys of the tuple
 *)
let bstree_of_list (lst: ('k *'v) list) ~compare:f =
  let rec aux lst' acc =
    match lst' with
    | [] -> acc
    | (k,v)::t -> aux t (insert acc ~compare:f k v)
  in aux lst L 

(* Tests *)
let () = printf "TESTS\n"
let () = printf "-------------------------------------------------------------\n"
let tuple_list = [(3,"three");(2,"two");(7,"seven");(6,"six");(8,"eight")]

let () = printf "\nConvert list to a binary search tree:\n"
let bs_tree = bstree_of_list tuple_list ~compare:Int.compare

let () = printf "\nInsert (1, \"one\"), returns tree with node 1:\n"

let insert_tree = insert bs_tree ~compare:Int.compare 1 "one"

let () = printf "\nFind key 6, returns \'Some \"six\"\':\n"

let find_6_test= find bs_tree ~compare:Int.compare 6

let () = printf "\nFind key 9, returns \'None\':\n"

let find_9_test = find bs_tree ~compare:Int.compare 9

let () = printf "\nDelete key 6, returns tree without node 6:\n"

let delete_6_test = delete bs_tree ~compare:Int.compare 6

let () = printf "\nDelete key 9, returns tree unchanged:\n"

let delete_9_test = delete bs_tree ~compare:Int.compare 9