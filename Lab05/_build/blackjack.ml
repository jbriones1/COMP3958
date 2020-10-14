open Base
open Stdio

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

(* Rounds within one hand.
 *
 * Init is the round where the player and dealer each receive their cards.
 * Player is where the game gets players commands to either hit or stay.
 *     If the player stays, jumps to Dealer.
 *     If the player busts, jumps to End.
 * Dealer is the round where the dealer has their turn. Jumps to End.
 * End is the round where a winner is checked. Jumps to Init.
 * Exit is where the program prints the final scores and exits.
 *)
type round =
  | Init
  | Player
  | Dealer
  | End
  | Exit

(* Represents the game state of both the player and the dealer.
 * 
 * round is the current round of the game.
 * p_hand is the player's hand.
 * d_hand is the dealer's hand.
 * deck is the current deck permutation.
 * p_points is the amount of points the player has.
 * d_points is the amount of points the dealer has.
 *)
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
  | Player    -> 1
  | Dealer    -> 2
  | End       -> 3
  | Exit      -> 4

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
(* Compares cards to each other, first by rank and then by suit 
 * Jack is 10, Queen is 11, King is 12, Ace is 14
 * Suits go Diamond < Club < Heart < Spade 
 * 
 * Card(r1, s1): The rank (r1) and suit (s1) of the first card
 * Card(r2, s2): The rank (r2) and suit (s2) of the second card 
 *)
let compare (Card (r1, s1)) (Card (r2, s2)) =
  let v1, v2 = int_of_rank r1, int_of_rank r2 in
  if v1 <> v2 
  then v1 - v2
  else int_of_suit s1 - int_of_suit s2

(* Reads the first character of the line that is input and returns it.
 * If end-of-file is input, ends the program.
 *)
let read_input () =
  match In_channel.input_line stdin with
  | None -> '\x00'
  | Some command -> 
    if String.length command < 1
    then '\t'
    else command.[0]

(* Compares the ranks of two cards. 
 * Used to check if two cards are equal, primarily.
 *
 * r1: is the first rank being compared
 * r2: is the second rank being compared
 *)
let r_compare r1 r2 =
  let i, j = int_of_round r1, int_of_round r2 in
  phys_equal i j

(*******************************************************************************
 GAME FUNCTIONS
 ******************************************************************************)
(* Counts the value of the hand, treating aces as 1 
 *
 * hand: the card list representing the hand
 *
 *
 * aux is the helper function to ensure tail recursion
 * 
 * acc: accumulator for hand value
 * lst' is the card list representing the hand
 *)
let check_ace hand =
  let rec aux acc lst' =
    match lst' with
    | [] -> acc
    | Card (rank, _)::t ->
      let r = int_of_rank rank in
      if (r = 11 || r = 12 || r = 13) then aux (acc+10) t
      else if (r = 14) then aux (acc+1) t
      else aux (acc+r) t
  in aux 0 hand

(* Counts the value of the hand, treating aces as 11
 *
 * hand: the card list representing the hand
 *
 *
 * aux is the helper function to ensure tail recursion
 * 
 * acc: accumulator for hand value
 * lst' is the card list representing the hand
 *)
let hand_sum hand =
  let rec aux acc lst' =
    match lst' with
    | [] -> acc
    | Card (rank, _)::t ->
      let r = int_of_rank rank in
      if (r = 11 || r = 12 || r = 13) then aux (acc+10) t
      else if (r = 14) then aux (acc+11) t
      else aux (acc+r) t
  in 
  let value = aux 0 hand in
  if value > 21
  then (check_ace hand)
  else value

(* Deals two cards to the player and one card to the dealer at the beginning of
 * the round. Does not use 'hit' due to the round structure needing to be
 * preserved. 
 *
 * state: game state
 * player: which player the game is dealing to
 *)
let rec first_deal state player = 
  let card = List.hd_exn state.deck in
  let deck' = List.tl_exn state.deck in
  if (String.equal player "player" 
      && r_compare state.round Init 
      && List.length state.p_hand = 0) then (* first card for player *)
  let state' =
    {
      round    = Init;
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
      round    = Init;
      p_hand   = card::state.p_hand;
      d_hand   = state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    print_endline @@ (string_of_card card) ^ " dealt to player";
    printf "Player at %d\n\n" (hand_sum state'.p_hand);
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
    printf "Dealer at %d\n" (hand_sum state'.d_hand);
    state'

(* Command for the player or dealer to hit
 * Checks the round state to see if it's the player or dealer's turn.
 * 
 * state: the current game state
 *)
let hit state =
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
  print_endline @@ (string_of_card card) ^ " dealt to player.\n" ^
  "Player at " ^ Int.to_string (hand_sum state'.p_hand);
  (* printf "Player at %d\n" (hand_sum state'.p_hand); *)
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
  printf "Dealer at %d\n" (hand_sum state'.d_hand);
  state'

(* Game state where the player hits 'Stay'
 * Changes the round to Dealer 
 *)
let stay state =
  {
    round    = Dealer;
    p_hand   = state.p_hand;
    d_hand   = state.d_hand;
    deck     = state.deck;
    p_points = state.p_points;
    d_points = state.d_points
  }

(*******************************************************************************
 INITIAL
 ******************************************************************************)
(* Creates all the cards for each suit 
 *
 * s is the suit
 *)
let cards_of_suit s =
  (List.map ~f:(fun x -> Card (Num x, s)) @@ List.range 2 11) @
  (List.map ~f:(fun x -> Card (x, s)) [Jack;Queen;King;Ace])

(* A list of all the cards in the deck
 *
 * s is the suit
 *)
let full_deck =
  List.fold_left ~init:[] ~f:(fun acc x -> acc @ cards_of_suit x)
    [Spade;Heart;Diamond;Club]

(* The initial state at the start of each game. *)
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
(* Represents the player's turn and its possible outcomes.
 * Once the game round is done, moves onto the Dealer round.
 * If the player busts, skip the the End round.
 *
 * state: the current game state
 *)
let rec player_turn state =
  let player_state =
  {
    round    = Dealer;
    p_hand   = state.p_hand;
    d_hand   = state.d_hand;
    deck     = state.deck;
    p_points = state.p_points;
    d_points = state.d_points
  } in
  if hand_sum state.p_hand > 21 (* Player busts *)
  then 
  let state' = 
  {
    round    = End;
    p_hand   = player_state.p_hand;
    d_hand   = player_state.d_hand;
    deck     = player_state.deck;
    p_points = player_state.p_points;
    d_points = player_state.d_points
  } in
  print_endline "You busted";
  state'
  else 
  let command = read_input () in
  if compare_char command 'H' = 0 || compare_char command 'h' = 0 then (* Hits *) 
    player_turn @@ hit state
  else if compare_char command 'S' = 0 || compare_char command 's' = 0 (* Stays *)
  then let state' = player_state in
    print_endline "Stay";
    state'
  else if compare_char command '\x00' = 0 then (* Exits *)
    {
      round    = Exit;
      p_hand   = state.p_hand;
      d_hand   = state.d_hand;
      deck     = state.deck;
      p_points = state.p_points;
      d_points = state.d_points
    }
  else let state' = state in (* Invalid input *)
    print_endline "Invalid input";
    player_turn state'

(* Represents the dealer's turn and its possible outcomes.
 * Dealer has some intelligence. Continiously hits. Once its hand value is 17 
 * or over, it will stay. If the dealer gets over a hard 21, it busts. 
 * 
 * state: the current game state
 *)
let rec dealer_turn state =
  let deal_state =
  {
    round    = Dealer;
    p_hand   = state.p_hand;
    d_hand   = state.d_hand;
    deck     = state.deck;
    p_points = state.p_points;
    d_points = state.d_points
  } in
  if hand_sum state.d_hand > 21
  then 
  let state' = 
  {
    round    = End;
    p_hand   = deal_state.p_hand;
    d_hand   = deal_state.d_hand;
    deck     = deal_state.deck;
    p_points = deal_state.p_points;
    d_points = deal_state.d_points
  } in
  print_endline "Dealer busts";
  state' 
  else if hand_sum state.d_hand >= 17
  then 
  let state' = 
  {
    round    = End;
    p_hand   = deal_state.p_hand;
    d_hand   = deal_state.d_hand;
    deck     = deal_state.deck;
    p_points = deal_state.p_points;
    d_points = deal_state.d_points
  } in
  print_endline "Dealer stays";
  state' 
  else 
  dealer_turn @@ hit state

(* Checks who won the round and awards one point to the winner. 
 * If the either player hits more than 21, they lose. If they both
 * are below 21, the higher value hand wins.
 * If they tie, no points are awarded to either player.
 * Also resets the deck to a shuffled and complete state.
 *)
let check_winner state =
  let state' =
  {
    round    = Init;
    p_hand   = state.p_hand;
    d_hand   = state.d_hand;
    deck     = List.permute full_deck;
    p_points = state.p_points;
    d_points = state.d_points
  } in
  if hand_sum state'.p_hand > 21 
     || (hand_sum state'.d_hand <= 21 
        && hand_sum state'.d_hand > hand_sum state'.p_hand)then 
    let state'' = 
    {
    round    = state'.round;
    p_hand   = [];
    d_hand   = [];
    deck     = state'.deck;
    p_points = state'.p_points;
    d_points = state'.d_points+1
  } in
  printf "(%d vs %d) Dealer wins\nPlayer %d | Dealer %d\n" 
    (hand_sum state'.p_hand) (hand_sum state'.d_hand)
    state''.p_points state''.d_points;
  state''
  else if hand_sum state'.d_hand > 21 
     || (hand_sum state'.p_hand <= 21 
        && hand_sum state'.d_hand < hand_sum state'.p_hand) then
    let state'' = 
      {
      round    = state'.round;
      p_hand   = [];
      d_hand   = [];
      deck     = List.permute full_deck;
      p_points = state'.p_points+1;
      d_points = state'.d_points;
    } in
    printf "(%d vs %d) Player wins\nPlayer %d | Dealer %d\n" 
      (hand_sum state'.p_hand) (hand_sum state'.d_hand)
      state''.p_points state''.d_points;
    state''
  else
  let state'' = 
    {
      round    = state'.round;
      p_hand   = [];
      d_hand   = [];
      deck     = List.permute full_deck;
      p_points = state'.p_points;
      d_points = state'.d_points;
    }  in
    printf "(%d vs %d) Tie\nPlayer %d | Dealer %d\n" 
      (hand_sum state'.p_hand) (hand_sum state'.d_hand)
      state''.p_points state''.d_points;
    state''

(* Prints both the player and dealer's scores *)
let print_scores state =
  printf "Player %d | Dealer %d\n" state.p_points state.d_points

(* Main driver of the program. 
 * Controls when a state is changed, based on the state it receives.
 * Recursively calls itself until the 'Exit' round state is given 
 *)
let rec controller state =
  match state.round with
  | Init -> 
    let state' = first_deal state "player" in
    print_endline "Done dealing. Hit or stay?";
    controller state'
  | Player -> 
    let state' = player_turn state in
    print_endline "Player turn done\n";
    controller state'
  | Dealer ->
    print_endline "Dealer's turn\n"; 
    let state' = dealer_turn state in
    print_endline "Dealer turn done\n";
    controller state'
  | End ->
    let state' = check_winner state in
    print_endline "\nNew Round------------------------------------------------";
    controller state'
  | Exit -> print_scores state

(*******************************************************************************
 MAIN FUNCTION
 ******************************************************************************)
(* Entry point to the blackjack game *)
let play () =
  print_endline 
    "Welcome to Blackjack. Press 'H' to hit and 'S' to stay. Quit with end-of-file";
  controller pregame_state;
  print_endline "Finished Blackjack. Goodbye"

let () = play ()
