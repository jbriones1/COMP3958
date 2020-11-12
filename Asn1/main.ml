(* 
 * Compares the frequencies of the frequencies list in the form (string, int).
 * If the frequencies are the same it sorts them by string.
 *
 * s1: first string
 * s2: second string
 * y1: the first frequency
 * y2: the second frequency
 *)
let freq_compare (s1, y1) (s2, y2) =
  let cmp = Base.Int.compare y1 y2 in
  if cmp = 0 then String.compare s1 s2
  else cmp
(* 
 * Uses the fold_left function to count the frequency of unique strings.
 * Sorts them using the freq_compare function
 *
 * lst: the list being processed
 * tup: tuple being checked being checked
 * n: the character string being checked
 * acc: accumulator that builds up a list of (string, int)
 *)
let frequencies_fold lst =
  let open Base in
  (* increments if the tuple matches, otherwise starts a new tuple *)
  let increment_if_match tup n =
    match tup with
    | (k, v) -> 
      if String.equal k n 
      then [(k, v+1)] 
      else [(n,1);(k,v)] in
  (* increments the value of the tuple *) 
  let increment acc n =
    match acc with
    | [] -> (n, 1)::acc
    | h::t -> (increment_if_match h n) @ t; in 

  List.fold_left ~init:[] ~f:increment lst |> List.sort ~compare: freq_compare

(* 
 * Finds the frequencies of the characters in the string
 * 
 * str: string to find frequencies of
 *)
let calc_frequencies str =
  (* Break the string into a list of string made of its characters *)
  let str_list = 
    String.to_seq str |> List.of_seq |> List.sort Char.compare 
    |> List.map (String.make 1) in
  frequencies_fold str_list

(*
 * Encodes the text using the provided huffman codes. Raises an exception if
 * one of the characters has no matching code.
 *
 * text: the string to encode
 * codes: the list of codes to use in the form ("char", "code")
 *)
let encode_text text codes =
  let list_of_text = 
    String.to_seq text |> List.of_seq |> List.map (String.make 1) in
  Base.List.fold_left list_of_text ~init:"" 
    ~f:(fun acc x ->  acc ^ (List.assoc x codes))

(*
 * Prints out a (string * int) list
 *
 * l: l is the list to print
 *)
let print_list l =
  List.iter (fun (x, y) -> Printf.printf "(%s, %d) " x y) l

(*
 * Prints out a (string * string) list
 *
 * l: l is the list to print
 *)
let print_list2 l = 
  List.iter (fun (x, y) -> Printf.printf "(%s, %s) " x y) l

(*
 * Removes the '\n' characters from a string
 *)
let remove_newlines str =
  let split = String.split_on_char '\n' str in
  Base.List.fold_left ~init:"" ~f:(^) split
(* 
 * Main function
 * Gets input from the user and removes new lines.
 * Calculates the frequencies from the input string then finds the Huffman
 * codes. Finally, it encodes the string based on the Huffman codes calculated.
 * Prints out each step.
 *)
let main () =
  let open Stdio in

  let input_text = remove_newlines @@ In_channel.input_all stdin in

  let freq = calc_frequencies input_text in

  let codes = Frequency_tree.huffman_of_list freq in

  let encoded_text = encode_text input_text codes in

  print_string @@ "Input string: " ^ input_text ^ "\nFrequency list: \n";
  print_list freq; print_endline "\nHuffman codes: ";
  print_list2 codes; print_endline "\nEncoded text (length):";
  printf "%s (%d)\n" encoded_text (String.length encoded_text)

(* Driver *)
let () = main ()


