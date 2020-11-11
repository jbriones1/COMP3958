include Frequency_tree
let freq_compare (_, y1) (_, y2) =
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

let print_list l =
  List.iter (fun (x, y) -> Printf.printf "(%s, %d) " x y) l

let print_list2 l = 
  List.iter (fun (x, y) -> Printf.printf "(%s, %s) " x y) l
let text = 
  "A_DEAD_DAD_CEDED_A_BAD_BABE_A_BEADED_ABACA_BED"
let huffman_codes = 
  [("_","00");("A","10");("B","1111");("C","1110");("D","01");("E","110")]


(* MAIN FUNCTION *)
open Printf

let () = printf "%s\n" text
let a = calc_frequencies text
let () = print_list a; print_endline ""