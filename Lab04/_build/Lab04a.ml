(* Question 1 *)
open Stdio
open Base

let () = 
  let argv = Sys.get_argv () in
  if Array.length argv = 1 then
    eprintf "Usage: %s filename" argv.(0)
  else
    let open In_channel in
    let c = create "data" in
    let s = input_lines c in
    let rec print_file_lines count lst acc =
      match lst with
      | [] -> printf "Total length of all lines: %d\n" acc
      | h::t -> printf "%d  %s  (Length): %d\n" count h (String.length h);
                print_file_lines (count+1) t (acc+String.length h)
    in print_file_lines (1) (s) (0);
    close c

                 