module M = Map.Make(Char)

type t = N of bool * t M.t

let empty = N (false, M.empty)

(* 
 * Takes a string and converts it to a list of its characters in lowercase.
 *
 * str: string to convert
 *
 * aux -- Tail-recursive helper function. Starts from the end of the string.
 * i: index of the string to put into the list
 * acc: list of characters representing the string
 *)
let charlist_of_string str =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (Char.lowercase_ascii str.[i]::acc) in
  aux (String.length str - 1) []

(* 
 * Takes a string and adds it to the list of characters. 
 *)
let add str tr =
  let char_list = charlist_of_string str in
  let rec add' ch tr' =
    match ch, tr' with
      [], N (_, m) -> N (true, m)
    | h::t, N (b, m) when M.mem h m -> N (b, M.add h (add' t (M.find h m)) m)
    | h::t, N (b, m) -> N (b, M.add h (add' t empty) m)
  in add' char_list tr

let find str tr = 
  let char_list = charlist_of_string str in
  let rec find' ch tr' =
    match ch, tr' with
      [], N (b, _) -> b
    | h::t, N (b, m) -> 
      if M.mem h m then find' t (M.find h m)
      else false
  in find' char_list tr

let count tr =
  let chk_word t = if t then 1 else 0 in
  let rec count' (N (b, l)) acc =
    M.fold (fun _ (N (b', _) as tr') acc -> (count' tr' (acc + (chk_word b')))) l acc
  in count' tr 0

let rec traverse (N(e, m)) f =
  M.iter (fun k (N(e', m')) -> f k e'; traverse (N(e', m')) f) m

let print tr =
  let open Stdio in
  traverse tr (
    fun k e ->
      if e then printf "%c\n" k
      else printf "%c" k
  )

let of_list lst = 
  Base.List.fold_left ~init:empty ~f:(fun acc x -> add x acc) lst