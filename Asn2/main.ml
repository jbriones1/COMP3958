let rec check_words str dictionary =
  let open Stdio in
  let str_lst = Base.String.split_on_chars ~on:[' '; '\n'] str in
  let rec aux str =
    match str with
      [] -> true
    | h::t -> My_trie.find h dictionary

let build_dictionary  =
  let open Stdio in
  let open In_channel in
  let file = create Sys.argv.(1) in
  let data = input_lines file in
  My_trie.of_list data

let main () =
  let open Stdio in
  let dictionary = build_dictionary in
  eprintf "Number of words: %d\n" @@ My_trie.count dictionary;
  let inp = read_line () in
  match My_trie.find inp with
    false -> eprintf "%s "

let () = main ()
