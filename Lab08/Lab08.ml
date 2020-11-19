(* 
 * Jon Andre Briones 
 * A01170906
 * Set 3V
 * Lab 08
 *)

type color = R | B
type 'a rbtree = L | N of color * 'a rbtree * 'a * 'a rbtree

let rec lookup t x =
  match t with
  | L -> false
  | N (_, l, y, r) ->
    x = y || (x < y && lookup l x) || (x > y && lookup r x)

let balance = function
  | B, N (R, N (R, a, x, b), y, c), z, d
  | B, N (R, a, x, N (R, b, y, c)), z, d   
  | B, a, x, N (R, N (R, b, y, c), z, d)
  | B, a, x, N (R, b, y, N (R, c, z, d))
    -> N (R, N (B, a, x, b), y, N (B, c, z, d))
  | c, l, x, r
    -> N (c, l, x, r)

let insert t x =
  let rec ins tr =
    match tr with
    | L -> N (R, L, x, L)
    | N (c, l, y, r) ->
      if x < y then balance (c, ins l, y, r)
      else if x > y then balance (c, l, y, ins r)
      else tr
  in
  match ins t with
  | L -> failwith "impossible"
  | N (_, l, y, r) -> N (B, l, y, r)

let rbtree_of_list =
  Base.List.fold_left ~init:L ~f:insert

(* 
 * Checks that a red node does not have red children.
 * 
 * If a node is red, it checks if its children are red and returns false.
 * Otherwise, it recursively checks its children.
 *)
let rec no_adjacent_reds : 'a rbtree -> bool = function
  | L -> true
  | N (R, N (R, _, _, _), _, _) | N (R, _ , _, N (R, _, _, _)) -> false
  | N (_, l, _, r) -> no_adjacent_reds l && no_adjacent_reds r

(* 
 * Counts the black height of each path and ensures it's equal on all paths.
 * Returns true if the left and right black-heights are equivalent.
 *
 * count_blacks: 
 * Tail-recursive black node counter
 * 
 * acc: the accumulator to track the number of black nodes
 * tr: the tree to count
 *)
let same_black_count : 'a rbtree -> bool = function
  | L -> true
  | N (_, l, _, r) ->
    let rec count_blacks acc tr =
      match tr with
      | L -> acc
      | N (R, l, _, r) -> (count_blacks acc l) + (count_blacks acc r)
      | N (B, l, _, r) -> (count_blacks (acc+1) l) + (count_blacks (acc+1) r)
    in (count_blacks 0 l = count_blacks 0 r)

(* 
 * Prints a red-black tree in ASCII code.
 * Uses ANSI escape codes to colour red nodes.
 *
 * tr: the tree to print
 * 
 * aux_print: 
 * Recursively print the red-black tree
 *
 * offset: the amount of spaces to offset different tiers of tree
 * tr': the tree to print
 *)
let rbt_print_ascii tr =
  let open Printf in
  match tr with
  | L -> print_endline "-"
  | _ -> 
    let rec aux_print offset tr' =
      match tr' with
      | L -> printf ("%s-\n") offset
      | N (B, l, x, r) -> 
        printf "%s%d\n" offset x;
        aux_print (offset ^ "  ") l; aux_print (offset ^ "  ") r
      | N (R, l, x, r) -> 
        printf "%s\027[31m%d\027[0m\n" offset x;
        aux_print (offset ^ "  ") l; aux_print (offset ^ "  ") r
    in aux_print "" tr

let bbb_tree = N (B, N (B, L, 1, L), 2, N (B, L, 3, L))
let rbb_tree = N (B, N (R, L, 1, L), 2, N (B, L, 3, L))
let rrb_tree = N (R, N (R, L, 1, L), 2, N (B, L, 3, L))
let rrr_tree = N (R, N (R, L, 1, L), 2, N (R, L, 3, L))
let brr_tree = N (R, N (B, L, 1, L), 2, N (R, L, 3, L))
let bbr_tree = N (B, N (B, L, 1, L), 2, N (R, L, 3, L))
let brb_tree = N (R, N (B, L, 1, L), 2, N (B, L, 3, L))
let rbr_tree = N (B, N (R, L, 1, L), 2, N (R, L, 3, L))
let print_tree = rbtree_of_list [3;2;7;6;8]

let () = print_endline @@ "\n\nTests are in the form left-root-right" ^
                          "\nTesting no adjacent reds" ^
                          "\n-------------------------------------"


let adj_red_test_bbb = no_adjacent_reds bbb_tree
let adj_red_test_rbb = no_adjacent_reds rbb_tree
let adj_red_test_rrb = no_adjacent_reds rrb_tree
let adj_red_test_rrr = no_adjacent_reds rrr_tree
let adj_red_test_brr = no_adjacent_reds brr_tree
let adj_red_test_bbr = no_adjacent_reds bbr_tree
let adj_red_test_brb = no_adjacent_reds brb_tree
let adj_red_test_rbr = no_adjacent_reds rbr_tree
let adj_red_test_print_tree = no_adjacent_reds print_tree

let () = print_endline @@ "\nTesting same black count" ^
                          "\n-------------------------------------"

let same_black_count_test_bbb = same_black_count bbb_tree 
let same_black_count_test_rbb = same_black_count rbb_tree 
let same_black_count_test_rrb = same_black_count rrb_tree 
let same_black_count_test_rrr = same_black_count rrr_tree 
let same_black_count_test_brr = same_black_count brr_tree 
let same_black_count_test_bbr = same_black_count bbr_tree 
let same_black_count_test_brb = same_black_count brb_tree 
let same_black_count_test_rbr = same_black_count rbr_tree 
let same_black_count_test_print_tree = same_black_count print_tree

let () = print_endline @@ "\nTesting tree print with [3;2;7;6;8]" ^
                          "\n-------------------------------------"

let () = rbt_print_ascii print_tree