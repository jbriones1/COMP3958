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
  p_points: int;
  d_points: int;
  deck: card list;
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

let cards_of_suit s =
  (List.map ~f:(fun x -> Card (Num x, s)) @@ List.range 2 11) @
  (List.map ~f:(fun x -> Card (x, s)) [Jack;Queen;King;Ace])

let full_deck =
  List.fold_left ~init:[] ~f:(fun acc x -> acc @ cards_of_suit x)
    [Spade;Heart;Diamond;Club]


let read_input () =
  match In_channel.input_line stdin with
  | None -> 'E'
  | Some command -> command.[0]

(*******************************************************************************
 GAME FUNCTIONS
 ******************************************************************************)
let rec get_hand_sum acc = function
  | [] -> acc
  | h::t ->
    let r = int_of_rank h in
    if (r = 11 || r = 12 || r = 13) then get_hand_sum (10+acc) t
    else get_hand_sum (r+acc) t

let p_deal state deck =
  match deck with
  | [] -> failwith "Empty deck"
  | h::t ->
  {
    round = Player;
    p_hand = h::state.p_hand;
    d_hand = state.d_hand;
    p_points = state.p_points;
    d_points = state.d_points;
    deck = t; 
  }

let d_deal state deck =
  match deck with
  | [] -> failwith "Empty deck"
  | h::t ->
  {
    round = Player;
    p_hand = state.p_hand;
    d_hand = h::state.d_hand;
    p_points = state.p_points;
    d_points = state.d_points;
    deck = t; 
  }


(*******************************************************************************
 STATES 
 ******************************************************************************)
let player_turn state =
  match state.deck with
  | [] -> failwith "Empty deck"
  | x::t -> print_string "Hit ('h') or stay ('s')";
    {
      round = Player;
      p_hand = x::state.p_hand;
      d_hand = state.d_hand;
      p_points = state.p_points;
      d_points = state.d_points;
      deck = t 
    }

let dealer_turn state =
  if state.d_points > 21
  then 
  {
    round = Init;
    p_hand = [];
    d_hand = [];
    p_points = state.p_points + 1;
    d_points = state.d_points;
    deck = state.deck; 
  }
  else if state.d_points >= 17 then
  {
    round = Dealer;
    p_hand = state.p_hand;
    d_hand = state.d_hand;
    p_points = state.p_points;
    d_points = state.d_points;
    deck = state.deck; 
  }
  else
  {
    round = Dealer;
    p_hand = state.p_hand;
    d_hand = state.d_hand;
    p_points = state.p_points;
    d_points = state.d_points;
    deck = state.deck; 
  }

let rec check_round state =
  match state.round with
  | Init -> check_round state
  | Player -> player_turn state
  | Dealer -> dealer_turn state
  | End -> 



(* MAIN FUNCTION *)
let rec play () =
  let game_state = {
    round = Player;
    p_hand = [];
    d_hand = [];
    p_points = 0;
    d_points = 0;
    deck = List.permute full_deck; 
  } in
  let rec check_command (state: state) =
    match read_input() with
    | 'h' | 'H' -> player_turn game_state
    | 's' | 'S' -> dealer_turn game_state
    | _ -> print_endline "Invalid input"; game_state
  in check_command game_state