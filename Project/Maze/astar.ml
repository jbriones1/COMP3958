(** 
    Implementation of A* based on the pseudo-code found in these sources:
    https://www.geeksforgeeks.org/a-search-algorithm/
    https://en.wikipedia.org/wiki/A*_search_algorithm
*)

module Coordinates =
struct
  type t = int * int
  let compare (x0, y0) (x1, y1) =
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1 
    | x -> x
end

module CoordMap = Map.Make(Coordinates)
module CoordSet = Set.Make(Coordinates)

(* 
  Heuristic formula. 
  Manhattan Distance used, due to the pathfinder only being allowed to move in 
  four directions. Finds the total distance by only moving horizontally and 
  vertically (no diagonals). 
*)
let h (x0, y0) (x1, y1) = abs(x0 - x1) + abs(y0 - y1)

(* 
  Finds the g cost of a node in the map.
  If the node is not in the map, return a number representing infinity.
*)
let g g_cost node =
  match CoordMap.find_opt node g_cost with
    Some v -> v
  | None -> max_int

(*  
  Open list is all the nodes immediately adjacent to areas that have been
  explored already.
  Closed list is the is a record of all nodes which have been explored
  and evaluated.
*)
let solve start target maze =
  let height = Array.length maze in
  let width = Array.length maze.(0) in

  (* 
    Set representing the open list. 
    Contains all the nodes immediately adjacent to the nodes already 
    explored.

    Each element is a coordinate.
  *)
  let open_set = CoordSet.add start CoordSet.empty in

  (* 
    Set representing the closed list.
    Contains a record of all the locations that have been explored.

    Each element is a coordinate.
  *)
  let closed_set = CoordSet.empty in


  (*
    Initialize a map that tracks all the g costs for nodes with the cost from
    the starting node. Since we start at the starting node, this initial cost
    is 0.

    Each element has:
    key: coordinate
    value: g cost
  *)
  let g_cost = CoordMap.add start 0  CoordMap.empty in

  (* 
    Initialize a map that tracks all f costs for nodes with the starting
    node's heurisitic cost.

    'f' is calculated by taking the heuristic cost and adding a 'g' cost

    Each element has:
    key: coordinate
    value: f cost
  *)
  let f_cost = CoordMap.add start (h target start) CoordMap.empty in 


  (* 
    A map of nodes with keys being a node and the values being the node that
    immediately precedes it on the shortest path.
  *)
  let from = CoordMap.empty in

  (*  
    Gets a list of coordinates in the four cardinal directions of the node in 
    the order:
    NW N NE
    W E
    SW S SE

    @return a list of nodes, representing adjacent nodes that are within the
            maze bounds
  *)
  let get_adjacents (x, y) =
    let adj_pos = 
      [(-1, -1); (0, -1); (1, -1);
       (-1, 0); (1, 0);
       (-1, 1); (0, 1); (1, 1)] in
    let all_adj = List.map (
        fun (x', y') -> 
          x + x', y + y'
      ) adj_pos in

    (* Ensures the nodes are within the bounds of the maze *)
    let adj = List.filter (
        fun (x, y )->
          x >= 0 && x < width && y >= 0 && y < height && maze.(y).(x) <> 0
      ) all_adj in
    adj
  in

  (* 
    Reconstructs the path from the current node to the start. 

    @return a list of nodes, representing the path taken from start to target
  *)
  let path_to_start from current =
    let rec aux acc curr =
      let from = CoordMap.find curr from in
      if from = start 
      then (from :: acc)
      else aux (from::acc) from
    in
    aux [current] current
  in

  (* 
    Gets the cost to enter a node.

    @return an int representing the cost to enter that node
  *)
  let cost (x, y) =
    maze.(y).(x)
  in

  (* 
    Implementation of the a_star algorithm.
    Loops through the open list and finds the lowest cost node in the list.

    If the open list is ever empty (no more paths to check), then the algorithm
    raises an exception.

    If the goal is the node found, then start backtracking from the target back 
    to the start and construct a list of the path taken.

    @return a list of nodes, representing the path taken from start to target
  *)
  let rec a_star (open_set, closed_set, f_cost, g_cost, from) =
    (* Find the coordinate with the smallest f cost in the open list*)
    if CoordSet.is_empty open_set 
    then failwith "No path found" 
    else
      let head =
        CoordSet.fold (
          fun n1 n2 -> 
            if n2 = (-1, -1) 
            then n1 
            else
              let f1 = CoordMap.find n1 f_cost in
              let f2 = CoordMap.find n2 f_cost in
              if f1 < f2 then n1 else n2
        ) open_set (-1, -1)
      in
      (* 
        If the head reaches the target, reconstruct the path from the head,
        otherwise keep looking through adjacent nodes to find a path.
      *)
      if head = target 
      then path_to_start from head
      else
        (* Moves the node from the visited list from the not visited list *)
        let open_set = CoordSet.remove head open_set in
        let closed_set = CoordSet.add head closed_set in

        let adj_nodes = get_adjacents head in

        List.fold_left (
          fun ((open_set, closed_set, f_cost, g_cost, from) as state) adj_node ->
            (* Skips nodes that have already been visited *)
            if CoordSet.mem adj_node closed_set 
            then state
            else
              let curr_g_cost = (g g_cost head) + (cost adj_node) in
              (* 
                Find the adjacent nodes with the lowest weight and updates the
                state.
              *) 
              if curr_g_cost < (g g_cost adj_node) 
              then
                let open_set = CoordSet.add adj_node open_set in
                let g_cost = CoordMap.add adj_node curr_g_cost g_cost in
                let f = (g g_cost adj_node) + (h adj_node target) in
                let f_cost = CoordMap.add adj_node f f_cost in
                let from = CoordMap.add adj_node head from in
                (open_set, closed_set, f_cost, g_cost, from)
                (* 
                  Skip the node if the cost isn't lower. 
                  Skips walls this way 
                *)
              else state
        ) (open_set, closed_set, f_cost, g_cost, from) adj_nodes
        |> a_star
  in
  a_star (open_set, closed_set, f_cost, g_cost, from)

(* 
  Converts the 2D int array of the maze to one represented by characters.
  Walls (#) are impassable and are represented by 0s.
  Normal terrain (.) has cost 1.
  Rough terrain (~) has cost greater than 1, but less than 5.
  Very rough terrain (^) is represented by anything greater than cost 5.

  maze: int maze to convert
  @return the character representation of the int maze
*)
let maze_int_to_char maze =
  let width = Array.length maze.(0) in
  let height = Array.length maze in

  Array.init height (
    fun y -> 
      Array.init width (
        fun x ->
          if maze.(y).(x) = 0 then '#'
          else if maze.(y).(x) > 4 then '^'
          else if maze.(y).(x) > 1 then '~'
          else '.'
      )
  )

(* 
  Modifies the maze and draws the path taken by the algorithm into the array.

  path: path to draw
  char_maze: character maze to draw the path on
  @return the character maze with the path drawn in
*)
let draw_path path char_maze =
  List.iter (
    fun (x, y) ->
      char_maze.(y).(x) <- '*'
  ) path;
  char_maze

(* 
  Prints the order of nodes the path finder went through 

  path: list of nodes to print
*)
let print_path path =
  List.iter (
    fun (x, y) ->
      Stdio.printf "(%d, %d)\n" x y
  ) path

(* 
  Prints the maze.

  maze: character maze to print
*)
let print_maze maze =
  Array.iter (
    fun row -> 
      Array.iter (
        fun node ->
          Stdio.printf "%c " node
      ) row;
      print_newline ()  
  ) maze;
  print_newline ()



(* ---------------Reading a Maze from a text file:------------------- *)
(*
  transform_lst
    - Transforms a list of strings into a list of list of ints
    - lst is the original list of strings
*)
let transform_lst lst =
  let split_string_to_ints s = 
    Core.String.split_on_chars ~on:[' '] s |> List.map int_of_string
  in
  let rec aux lst' acc =
    match lst' with
    | [] -> acc
    | h::t -> aux t (split_string_to_ints h::acc)
  in aux lst []

(*
    lst_to_arr
      - Converts a list of list of 'a to an array of array of 'a
      - lst is the list to be converted
*)
let lst_to_arr lst = 
  Array.of_list (List.map Array.of_list lst)

(*
  Process_Lines:
    - processes a line from the stdin with the passed in function f
*)
let rec process_lines f =
  match Stdio.In_channel.input_line stdin with
  | Some x -> f x
  | None -> process_lines f

(* 
  Str_to_tuple:
    - Takes a string that contains two ints separated by ' ' and converts it into a tuple
    - Returns a tuple consisting of the two ints in the string.
*)
let str_to_tuple str =
  let split_string_to_ints s = 
    Core.String.split_on_chars ~on:[' '] s |> List.map int_of_string
  in
  let str_list = split_string_to_ints str in
  let rec aux lst' (x, y) =
    match lst' with
    | [] -> (x, y)
    | h::[] -> (x, h)
    | h::t -> aux t (h, y)
  in aux str_list (-1, -1)

(*
    "Main" Function
      - Reads in a txt file containing the Maze.
      - Asks user input for starting coordinate & target coordinate.
      - Solves the maze, prints out the path.
*)
let () =
  
  (* Reading in the txt file containing the Maze. *)
  let maze = Stdio.In_channel.read_lines Sys.argv.(1) |> List.rev |> transform_lst |> lst_to_arr in

  (* Getting Starting Coordinate from stdin. *)
  Stdio.printf "%s\n" "Enter the starting coordinates like so: \"x y\"";
  Stdio.Out_channel.flush stdout;
  let start = process_lines str_to_tuple in 

  (* Getting Target Coordinate from stdin. *)
  Stdio.printf "%s\n" "Enter the target coordinates like so: \"x y\"";
  Stdio.Out_channel.flush stdout;
  let target = process_lines str_to_tuple in 

  (* Solving the maze. *)
  let path = solve start target maze in
  print_path path;

  maze_int_to_char maze |> draw_path path |> print_maze



