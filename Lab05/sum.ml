open Stdio

let rec print_lines () =
  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
    printf "%s" 