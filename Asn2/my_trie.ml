module M = Map.Make(Char)

type t = N of bool * t M.t

(* 
 * An empty trie.
 *)
let empty = N (false, M.empty)

(* 
 * Takes a string and adds it to a trie.
 * Breaks it apart into a list of characters and goes through the trie until it
 * reaches the end of the word. The end of the word is marked by making the node
 * true.
 * Uses add' as a recursive way to traverse the trie.
 *
 * str: string to add to the trie
 * tr: trie to add to
 * returns: trie with the added string
 *)
let add str tr =
  let char_list = Base.String.to_list str in
  let rec add' ch tr' =
    match ch, tr' with
      [], N (_, m) -> N (true, m)
    | h::t, N (b, m) when M.mem h m -> N (b, M.add h (add' t (M.find h m)) m)
    | h::t, N (b, m) -> N (b, M.add h (add' t empty) m)
  in add' char_list tr

(* 
 * Finds if a string exists in a trie.
 * Uses find' as a recursive way to traverse the trie.
 *
 * str: string to search for
 * tr: trie to search through
 * returns: true if the word exists, false otherwise
 *)
let find str tr = 
  let char_list = Base.String.to_list str in
  let rec find' ch tr' =
    match ch, tr' with
      [], N (b, _) -> b
    | h::t, N (_, m) -> 
      if M.mem h m then find' t (M.find h m)
      else false
  in find' char_list tr

(* 
 * Counts the number of words in the trie.
 * Uses count' to ensure tail recursion.
 * 
 * tr: trie to search through
 * returns: the number of words in the trie
 *)
let count tr =
  let chk_word t = if t then 1 else 0 in
  let rec count' (N (_, l)) acc =
    M.fold (
      fun _ (N (b', _) as tr') acc -> 
        (count' tr' (acc + (chk_word b')))
    ) l acc
  in count' tr 0

(*
 * Creates a trie from a list of strings.
 *
 * lst: list of strings to create from
 * returns: trie created from strings 
 *)
let of_list lst = 
  Base.List.fold_left ~init:empty ~f:(fun acc x -> add x acc) lst