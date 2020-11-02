module type My_monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end 

module List_monad : My_monad with type 'a t := 'a list = struct
  type 'a t = 'a list

  let return x = [x]

  let bind (xs:'a t) (f:'a -> 'b t) =
    let open Base in
    xs |> List.map ~f |> List.concat

  let (>>=) = bind
end

let guarded b xs =
  if b then xs
  else []

let multiply_to n =
  let open Base in
  let open List_monad in
  List.range ~stop:`inclusive 1 n >>= fun x ->
  List.range ~stop:`inclusive 1 n >>= fun y ->
  guarded (x * y = n) @@ return (x, y)

let pythag_triplets n =
  let open Base in
  let open List_monad in
  List.range ~stop:`inclusive 1 n >>= fun a ->
  List.range ~stop:`inclusive (a+1) n >>= fun b ->
  List.range ~stop:`inclusive (b+1) n >>= fun c ->
  guarded (a*a + b*b = c*c && a + b + c = n) 
    @@ return (a, b, c)
