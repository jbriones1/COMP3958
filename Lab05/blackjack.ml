open Base

type suit = Diamond | Club | Heart | Spade

type rank =
  Ace
  | King
  | Queen
  | Jack
  | Num of int

type card = Card of rank * suit

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

let compare (Card)