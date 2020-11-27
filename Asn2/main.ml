(* 
 * Checks if a line of words is in the dictionary.
 * Splits a line into words based on spaces and newline characters.
 * Creates a list of words that are not in the dictionary and then prints the
 * incorrect words in red with an underline.
 *
 * str: string to check
 * dictionary: dictionary of words to use
 *)
let check_words str dictionary =
  let open Stdio in
  let str_lst = Base.String.split_on_chars ~on:[' '; '\n'] str in
  let rec search_word acc str =
    match str with
      [] -> acc
    | h::t -> 
      if My_trie.find h dictionary
      then search_word acc t
      else search_word (h::acc) t in
  let rec print_wrong_words = function
      [] -> flush stderr
    | h::t -> eprintf "\027[31;4m%s\027[0m\ \n" h; 
      print_wrong_words t;
  in str_lst |> search_word [] |> print_wrong_words 

(* 
 * Reads the first argument of program initialization and creates a dictionary
 * creates a dictionary (trie) from it. Prints the number of words in the
 * dictionary.
 * 
 * returns: the dictionary
 *)
let build_dictionary  =
  let open Stdio in
  let open In_channel in
  let file = create Sys.argv.(1) in
  let data = input_lines file in
  let dictionary = My_trie.of_list data in
  eprintf "Number of words: %d\n" @@ My_trie.count dictionary;
  flush stderr;
  dictionary

(* Driver *)
let () =
  let dictionary = build_dictionary in
  let rec loop () =
    try 
      let inp = read_line () in
      check_words inp dictionary;
      loop ()
    with End_of_file -> Stdio.eprintf "Done\n";
      flush stderr
  in loop ()