module M = Map.Make(Int)

(* Power function 
 *
 * a is the base
 * n is the exponent *)
let pow a n =
  let rec pow' n acc =
    if n <= 0
    then acc
    else pow' (n-1) (a*acc)
  in pow' n 1

(* 
 * Splits a number into a list of its digits 
 *
 * n is the number being split 
 *)
let digits n =
  let open Base in
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ -> aux (n / 10) ((n % 10)::acc)
  in aux n []

(* 
 * Sorts a list of integers from lowest to highest and then combines them into
 * a single number using the list indices for the digits. Index 0 is the most
 * significant digit.
 * 
 * lst is the list of digits to sort from lowest to highest 
 *)
let sorted_digits_to_number n =
  let open Base in
  let s_lst = List.sort (digits n) ~compare: Int.compare in
  let rec to_number lst' acc =
    match lst' with
    | [] -> acc
    | h::t -> to_number t (acc*10 + h)
  in to_number s_lst 0

(* 
 * Filters any number below n digits from a list.
 *
 * n is the number of digits it will stop filtering at
 * lst is the list to filter numbers from 
 *)
let rec digit_filter n lst =
  match lst with
  | [] -> []
  | h::t -> 
    if h < (pow 10 (n-1))
    then digit_filter n t
    else h::t

(* 
 * Uses filter to remove all multiples of prime numbers.
 *
 * n is the positive integer the program will find M up to (inclusive)
 * lst is the list of numbers from 2 to n
 * acc is the accumulator to ensure tail recursion 
 *)
let primes n = 
  let open Base in
  let rec sieve lst acc =
    match lst with
    | [] -> (List.rev acc)
    | h::t -> 
      if h*h < n
      then sieve (List.filter t ~f:(fun x -> (x%h)<>0)) (h::acc)
      else h::t
  in sieve (List.range 2 (n+1)) []

let primes_list = digit_filter 6 @@ primes 1000000

(* 
 * Maps the primes, with keys being the digits of the primes and the value
 * being the number of times those digits pop up
 *
 * map: the map being added to
 * lst: the list of primes
 *)
let rec map_primes map lst =
  match lst with
  [] -> map
  | h::t ->
    let key = sorted_digits_to_number h in
      if (M.find_opt key map) <> None then
        let count = (M.find key map) in
        map_primes (M.add key (count+1) map) t
      else
        map_primes (M.add key 1 map) t

(* 
 * Finds the largest value in the map
 *
 * map: the map to search through
 *)
let find_largest map =
  M.fold (fun _ v acc -> max v acc) map 0

let largest_sequence = find_largest @@ map_primes M.empty primes_list