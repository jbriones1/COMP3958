open Base
open Stdio


(* UNFINISHED *)
(*******************************************************************************
 TYPE DECLARATION 
 ******************************************************************************)
type suit = Diamond | Club | Heart | Spade

type rank =
  | Ace
  | King
  | Queen
  | Jack
  | Num of int

type card = Card of rank * suit

type round =
  | Init
  | New_Round
  | Player
  | Dealer
  | End

type state = {
  round: round;
  p_hand: card list;
  d_hand: card list;
  deck: card list;
  p_points: int;
  d_points: int
}

let int_of_suit = function
  | Spade   -> 4
  | Heart   -> 3
  | Club    -> 2
  | Diamond -> 1

let int_of_rank = function
  | Ace   -> 14
  | King  -> 13
  | Queen -> 12
  | Jack  -> 11
  | Num x -> x

let int_of_round = function
  | Init      -> 0
  | New_Round -> 1
  | Player    -> 2
  | Dealer    -> 3
  | End       -> 4

let suit_of (Card(_, s)) = s

(*******************************************************************************
 PRINTING CARDS 
 ******************************************************************************)
let string_of_rank = function
  | Ace   -> "Ace"
  | King  -> "King"
  | Queen -> "Queen"
  | Jack  -> "Jack"
  | Num x -> Int.to_string x

let string_of_suit = function
  | Spade   -> "Spade"
  | Heart   -> "Heart"
  | Club    -> "Club"
  | Diamond -> "Diamond"

let string_of_card (Card (r, s)) =
  string_of_rank r ^ " of " ^ string_of_suit s

(*******************************************************************************
 AUXILLIARY FUNCTIONS 
 ******************************************************************************)
let compare (Card (r1, s1)) (Card (r2, s2)) =
  let v1, v2 = int_of_rank r1, int_of_rank r2 in
  if v1 <> v2 
  then v1 - v2
  else int_of_suit s1 - int_of_suit s2

let read_input () =
  match In_channel.input_line stdin with
  | None -> '\x00'
  | Some command -> 
    if String.length command < 1
    then '\t'
    else command.[0]

let r_compare r1 r2 =
  let i, j = int_of_round r1, int_of_round r2 in
  phys_equal i j

let l_length lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc
    | _::t -> aux t (acc+1)
  in aux lst 0

(*******************************************************************************
 GAME FUNCTIONS
 ******************************************************************************)
let hand_sum (lst: card list) =
  let rec aux acc (lst: card list) =
    match lst with
    | [] -> acc
    | Card (rank, _)::t ->
      let r = int_of_rank rank in
      if (r = 11 || r = 12 || r = 13) then aux (acc+10) t
      else if (r = 14) then aux (acc+1) t
      else aux (acc+r) t
  in aux 0 lst

let check_bust state =
  let hand_val =
  if (r_compare state.round Player) 
  then hand_sum state.p_hand
  else hand_sum state.d_hand in
  hand_val > 21

let rec first_deal state player = 
  let card = List.hd_exn state.deck in
  let deck' = List.tl_exn state.deck in
  if (String.equal player "player" 
      && r_compare state.round Init 
      && l_length state.p_hand = 0) then (* first card for player *)
  let state' =
    {
      round    = state.round;
      p_hand   = card::state.p_hand;
      d_hand   = state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    print_endline @@ (string_of_card card) ^ " dealt to player";
    first_deal state' "player"
  else if (String.equal player "player" (* Second card for player *)
          && r_compare state.round Init ) then
  let state' =
    {
      round    = state.round;
      p_hand   = card::state.p_hand;
      d_hand   = state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    print_endline @@ (string_of_card card) ^ " dealt to player";
    first_deal state' "dealer"
  else (* Dealer's first card *)
  let state' =
    {
      round    = Player;
      p_hand   = state.p_hand;
      d_hand   = card::state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    print_endline @@ (string_of_card card) ^ " dealt to dealer";
    state'

let rec hit state =
  let card = List.hd_exn state.deck in
  let deck' = List.tl_exn state.deck in
  if (r_compare state.round Player)
  then let state' =
  {
    round    = Player;
    p_hand   = card::state.p_hand;
    d_hand   = state.d_hand;
    deck     = deck';
    p_points = state.p_points;
    d_points = state.d_points
  } in
  print_endline @@ (string_of_card card) ^ " dealt to player";
  state'
  else let state' =
  {
    round    = Dealer;
    p_hand   = state.p_hand;
    d_hand   = card::state.d_hand;
    deck     = deck';
    p_points = state.p_points;
    d_points = state.d_points
  } in 
  print_endline @@ (string_of_card card) ^ " dealt to dealer";
  state'



let rec check_command () =
  match read_input () with
  | 'H' | 'h' -> print_endline "Hit"; check_bust state; 
  | 'S' | 's' -> print_endline "Stay"; check_command ()
  | '\x00' -> print_endline "Goodbye"
  | _ -> print_endline "Invalid input"; check_command ()

(*******************************************************************************
 INITIAL
 ******************************************************************************)
let cards_of_suit s =
  (List.map ~f:(fun x -> Card (Num x, s)) @@ List.range 2 11) @
  (List.map ~f:(fun x -> Card (x, s)) [Jack;Queen;King;Ace])

let full_deck =
  List.fold_left ~init:[] ~f:(fun acc x -> acc @ cards_of_suit x)
    [Spade;Heart;Diamond;Club]

let pregame_state = {
    round    = Init;
    p_hand   = [];
    d_hand   = [];
    deck     = (List.permute full_deck);
    p_points = 0;
    d_points = 0
  } 

(*******************************************************************************
 TURNS
 ******************************************************************************)
let rec controller state =
  match state.round with
  | Init -> 


(*******************************************************************************
 MAIN FUNCTION
 ******************************************************************************)
let play () =
  let game_state = first_deal pregame_state "player" in
  let () = print_endline "H to hit, s to stay" in

let () = play ()
