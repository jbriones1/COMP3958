open Stdio
open Base

(* Question 1 
 * Opens the files specified when running the program.
 * Shows the line numbers, contents of the lines and finds the total length of 
 * all the lines.
 * count is the line number count
 * lst is the list of lines from the file
 * acc is the file length counter *)
let () = 
  let argv = Sys.get_argv () in
  if Array.length argv = 1 then
    eprintf "Usage: %s filename" argv.(0)
  else
    let open In_channel in
    let c = create argv.(1) in
    let s = input_lines c in
    let rec print_file_lines count lst acc =
      match lst with
      | [] -> printf "Total length of all lines: %d\n" acc
      | h::t -> printf "%d  %s  (Length): %d\n" count h (String.length h);
                print_file_lines (count+1) t (acc+String.length h)
    in print_file_lines (1) (s) (0);
    close c

                 