
open Elements

type 'a t = unit -> 'a s
 and 'a s = :: of 'a * 'a t

let rec count n () = n :: count (n + 1)

let rec map f l () =
  match l () with
  | x :: k -> f x :: map f k

let rec take n l =
  match l () with
  | x :: k ->
    if n = 0 then []
    else List.cons x (take (n - 1) k)

let rec filter p l () =
    match l () with
      | (x :: k) ->
        if p x
        then x :: filter p k
        else      filter p k ()


