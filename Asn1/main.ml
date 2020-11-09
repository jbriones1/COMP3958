module type OrderedType = sig
  type t
  val compare: t -> t -> int
end

module type S = sig
  type elt
  type t
  val empty : t
  val merge : t -> t -> t
  val insert : t -> elt * int -> t
  val heap_of_list : elt list -> t
  val get_min : t -> elt option
  val delete_min : t -> t
  val heap_sort : elt list -> elt list
end

module Make(Ord: OrderedType) = struct
  type elt = Ord.t
  type t = E | T of int * elt * t * t

  let rank = function
    | E -> 0
    | T (r, _, _, _) -> r

  let empty = E

  let rec merge t1 t2 =
    match t1, t2 with
    | E, t | t, E -> t
    | T (_, x1, _, _), T(_, x2, _, _) when Ord.compare x2 x1 < 0 ->
      merge t2 t1
    | T (_, x, l, r), t ->
      let r' = merge r t in
      if rank l >= rank r' then T (1 + rank r', x, l, r')
      else T (1 + rank l, x, r', l)

  let insert t x = merge t (T (1, x, E, E))

  let heap_of_list l =
    let open Base in
    List.fold_left ~init:E ~f:(fun t x -> insert t x) l

  let get_min = function
    | E -> None
    | T (_, x, _, _) -> Some x

  let delete_min = function
    | E -> failwith "delete_min: empty tree"
    | T (_, _, l, r) -> merge l r

  let heap_sort lst =
    let h_list = heap_of_list lst in
    let rec aux lst' heap =
      match heap with
      | E -> List.rev lst'
      | T (_, x, _, _) ->  aux (x::lst') (delete_min heap) in
    aux [] h_list
end

module Frequency_tree = struct
  type t = E | T of (string * int) * t * t

  let compare t1 t2 =
    match t1, t2 with
    | E, E -> 0
    | t, E | E, t -> 0
    | (T((_, x1), _, _)), (T((_, x2), _, _)) -> Base.Int.compare x1 x2
end

module FT = Make(Frequency_tree)

let freq_compare (x1, y1) (x2, y2) =
  Base.Int.compare y1 y2

(* 
 * Uses the fold_left function to count the frequency of unique strings
 *
 * lst: the list being processed
 * tup: tuple being checked being checked
 * n: the character string being checked
 * acc: accumulator that builds up a list of (string * int)
*)
let frequencies_fold lst =
  let open Base in

  let increment_if_match tup n =
    match tup with
    | (k, v) -> 
      if String.equal k n 
      then [(k, v+1)] 
      else [(n,1);(k,v)] in 

  let increment acc n =
    match acc with
    | [] -> (n, 1)::acc
    | h::t -> (increment_if_match h n) @ t; in 

  List.fold_left ~init:[] ~f:increment lst |> List.sort ~compare: freq_compare

let calc_frequencies str =

  (* Break the string into a list of string made of its characters *)
  let str_list = 
    String.to_seq str |> List.of_seq |> List.sort Char.compare 
    |> List.map (String.make 1) in

  frequencies_fold str_list

let encode_text text codes =

  let list_of_text = 
    String.to_seq text |> List.of_seq |> List.map (String.make 1) in

  Base.List.fold_left list_of_text ~init:"" 
    ~f:(fun acc x ->  acc ^ (List.assoc x codes))

let text = 
  "A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED"
let huffman_codes = 
  [("_","00");("A","10");("B","1111");("C","1110");("D","01");("E","110")]



(* MAIN FUNCTION *)
let a = encode_text text huffman_codes
let () = Printf.printf "%s (%d)\n" a (String.length a)