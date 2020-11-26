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
let add str t =
  let char_list = charlist_of_string str in
  let rec ins ch tr =
    match ch, tr with
      [], N (_, m) -> N (true, m)
    | h::t, N (b, m) ->
      if M.mem h m then N (b, M.add h (ins t (M.find h m)) m)
      else N (b, M.add h (ins t empty) m)
  in ins char_list t

let find str t = 
  let char_list = charlist_of_string str in
  let rec traverse ch tr =
    match ch, tr with
      [], N (b, _) -> b
    | h::t, N (b, m) -> 
      if M.mem h m then traverse t (M.find h m)
      else false
  in traverse char_list t

let count tr = 
  let rec count' tr' acc =
    let chk_word b = if b then 1 else 0 in
    M.fold (fun _ t acc -> (count' t (acc + (chk_word b)))) m acc
  in count' tr 0