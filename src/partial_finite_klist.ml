
type 'a t =
  | Nil
  | Cons of ('a * (unit -> 'a t))

let rec fold f acc l =
  match l with
  | Nil -> acc
  | Cons (x, k) -> fold f (f acc x) (k ())

let rec count n =
  Cons (n, fun () -> (count (n + 1)))

let rec of_list l =
  match l with
  | [] -> Nil
  | x :: xs -> Cons (x, fun () -> of_list xs)

let rec init n f =
  let rec loop i =
    if i = n then Nil
    else Cons (f i, fun () -> loop (i + 1)) in
  loop 0

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons (x, l') -> Cons (f x, fun () -> map f (l' ()))

let rec filter p l =
  match l with
  | Nil -> Nil
  | Cons (x, l') when p x -> Cons (x, fun () -> filter p (l' ()))
  | Cons (_, l') -> filter p (l' ())

let rec take n l =
  if n = 0 then Nil
  else match l with
    | Nil -> Nil
    | Cons (x, l') -> Cons (x, fun () -> take (n - 1) (l' ()))

