
open Elements

type 'a t = unit -> 'a s
 and 'a s = :: of 'a * 'a t
          | Nil

let rec count n () = n :: count (n + 1)

let rec fold f acc l =
  match l () with
  | Nil -> acc
  | x :: k -> fold f (f acc x) k

let rec map f l () =
  match l () with
  | Nil -> Nil
  | x :: k -> f x :: map f k

let rec take n l () =
  match l () with
  | Nil -> Nil
  | x :: k ->
    if n = 0 then Nil
    else x :: (take (n - 1) k)

let to_list l =
  let rec loop acc l' =
    match l' () with
    | Nil -> acc
    | x :: k -> loop (List.cons x acc) k in
  List.rev (loop [] l)

let rec filter p l () =
    match l () with
      | Nil -> Nil
      | (x :: k) ->
        if p x
        then x :: filter p k
        else      filter p k ()


