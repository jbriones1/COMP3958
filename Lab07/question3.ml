let guarded b xs =
  if b then xs
  else []

(**
 * Question 3
 *
 * List out pythagorean triplets that equals the input integer, with 
 * a < b < c, where a, b and c are positive integers.
 * 
 * n: an integer the triplets add up to
 *)
let pythag_triplets n =
  let open Base in
  let open List in
  range ~stop:`inclusive 1 n >>= fun a ->
  range ~stop:`inclusive (a+1) n >>= fun b ->
  range ~stop:`inclusive (b+1) n >>= fun c ->
  guarded (a*a + b*b = c*c && a + b + c = n) 
    @@ return (a, b, c)

let question3_test = pythag_triplets 1000
