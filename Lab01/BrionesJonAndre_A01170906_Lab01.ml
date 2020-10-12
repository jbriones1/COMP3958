(* Jon Andre Briones
 * A01170906
 * Set 3V*)

open Printf;;

(* Question 1 *)
let rec power a n =
  if n <= 0 then 1.
  else a *. (power a (n - 1));;

printf "2 to the power of 3 is %f\n" (power 2. 3);;

(* Question 2 *)
let fact n =
  let rec fact' n a =
     if n <= 0 then a
     else fact' (n - 1) (n * a)
  in 
    fact' n 1;;

let f x n =
  (power x n) /. float_of_int(fact n);;

printf "x^2 / 2! is %f\n" (f 1. 2);;
printf "x^3 / 3! is %f\n" (f 1. 3);;
printf "x^20 / 20! is %f\n" (f 1. 20);;

(* Question 3 *) 
let exp x n =
  let rec exp' f i a =
    if n = (i - 1) then a
    else exp' (f *. x /. float_of_int(i)) (i+1) (f *. x /. float_of_int(i) +. a)
  in exp' 1. 1 1.;;

printf "exp(1) on the zeroeth term is %f\n" (exp 1. 0);;
printf "exp(1) on the first term is %f\n" (exp 1. 1);;
printf "exp(1) on the second term is %f\n" (exp 1. 2);;
printf "exp(1) on the third term is %f\n" (exp 1. 3);;
printf "exp(1) on the 20th term is %f\n" (exp 1. 20);;
