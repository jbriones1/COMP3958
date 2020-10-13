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

type round =
  | Init
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
  | None -> 'E'
  | Some command -> command.[0]

(*******************************************************************************
 GAME FUNCTIONS
 ******************************************************************************)
let rec hand_sum acc = function
  | [] -> acc
  | (rank, _)::t ->
    let r = int_of_rank rank in
    if (r = 11 || r = 12 || r = 13) then hand_sum (10+acc) t
    else hand_sum (r+acc) t

let rec first_deal state player =
  let card = List.hd_exn state.deck in
  let deck' = List.tl_exn state.deck in
  if (player = "player" && state.round = Init && state.p_hand = []) then
  let state' =
    {
      round    = state.round;
      p_hand   = card::state.p_hand;
      d_hand   = state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    first_deal state' "player"
  else if (player = "player" && state.round = Init) then
  let state' =
    {
      round    = state.round;
      p_hand   = card::state.p_hand;
      d_hand   = state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    } in
    first_deal state' "dealer"
  else
    {
      round    = Player;
      p_hand   = state.p_hand;
      d_hand   = card::state.d_hand;
      deck     = deck';
      p_points = state.p_points;
      d_points = state.d_points
    }
  

(*******************************************************************************
 CARD FUNCTIONS
 ******************************************************************************)
let cards_of_suit s =
  (List.map ~f:(fun x -> Card (Num x, s)) @@ List.range 2 11) @
  (List.map ~f:(fun x -> Card (x, s)) [Jack;Queen;King;Ace])

let full_deck =
  List.fold_left ~init:[] ~f:(fun acc x -> acc @ cards_of_suit x)
    [Spade;Heart;Diamond;Club]

(*******************************************************************************
 MAIN FUNCTION
 ******************************************************************************)
