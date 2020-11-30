type 'a stream = Cons of 'a * (unit -> 'a stream)

let rec from n = Cons (n, fun () -> from (n + 1))

let hd (Cons (h, _)) = h

let tl (Cons (_, t)) = t

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h::(take (n-1) @@ t ())

(*
 * Question 1a
 * 
 * Checks if the predicate is true or false.
 * Removes all values that do not satisfy the predicate.
 *
 * s: stream being filtered
 * f: the function that acts as the predicate
 * returns the modified stream
 *)
let rec filter s ~f = 
  match s with
    Cons (h, t) ->
    if f h then Cons (h, fun () -> filter ~f (t ()))
    else filter ~f (t ())

(*
 * Question 1b
 * 
 * Applies a function to the values of a stream.
 *
 * s: stream to be modified
 * f: function to apply to the stream
 * returns the modified stream
 *)
let rec map s ~f =
  Cons (f @@ hd s, fun () -> map (tl s ()) f)

(*
 * Question 2
 *
 * Sieve of Eratosthenes to filter out numbers that are multiples of other
 * numbers.
 * Uses the filter to remove all multiples of a number from the stream, with
 * sieve' being the predicate.
 * 
 * s: stream to be modified
 * returns the modified stream
 *)
let rec sieve s =
  match s with
    Cons (h, t) -> 
    let sieve' n =
      filter s (fun x -> x mod n <> 0)
    in Cons (h, fun () -> sieve (sieve' h))

(*
 * Question 3a
 *
 * Uses a stream of integers starting from 0 and calculates what the term
 * of the exponential series should equal, mapping it on the stream.
 * A tail recursive factorial function and a term calculator function are
 * implemented to help map the stream.
 *
 * fl: the exponent of e
 * x: the number to calculate the factorial of
 * acc: accumulator for tail-recursion
 * num: the (n-1)th term of the infinite exponential series
 *)
let rec exp_terms fl =
  let rec fact x acc =
    if x <= 1 then acc
    else fact (x - 1) (x * acc) in
  let rec term_calc num =
    fl ** float_of_int(num) /. float_of_int(fact num 1) in
  map (from 0) (fun x -> term_calc x)

(* 
 * Question 3b
 *
 * Fold function that takes the first n values of a stream and applies folds
 * over these values.
 *
 * s: stream to fold over
 * init: the accumulator
 * f: the function to fold with
 * n: the number of values to fold over
 *)
let rec foldn s ~(init: 'acc) ~f ~n =
  match s with
    Cons (h, t) ->
    if n < 1 then init else
      foldn ~init:(f init h) ~f ~n:(n - 1) (t ()) 


let () = Stdio.print_endline "********** TESTS **********"
let test_stream = from 1
let () = Stdio.print_endline "\nFilter function: Remove odds"
let () = Stdio.print_endline "-------------------------------------------------"
let filter_test = take 20 @@ filter ~f:(fun x -> x mod 2 = 0) test_stream
let () = Stdio.print_endline "\nMap function: Square each number"
let () = Stdio.print_endline "-------------------------------------------------"

let map_test = take 20 @@ map ~f:(fun x -> x*x) test_stream
let () = Stdio.print_endline "\nFirst 100 primes"
let () = Stdio.print_endline "-------------------------------------------------"

let first_100_primes = take 100 @@ sieve (from 2)
let () = Stdio.print_endline "\nApproximation of exp(1), with 20 terms"
let () = Stdio.print_endline "-------------------------------------------------"

let exp1 = foldn ~init: (0.) ~f:(+.) ~n:(20) (exp_terms 1.0)