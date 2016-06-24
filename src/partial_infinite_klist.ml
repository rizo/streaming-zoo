
type 'a t = (::) of 'a * (unit -> 'a t)

let rec count n = n :: fun () -> count (n + 1)

let rec map f (x :: k) = f x :: fun () -> map f (k ())

let rec take n (x :: k) =
  if n = 0 then []
  else List.cons x (take (n - 1) (k ()))

let rec filter p (x :: k) =
  if p x
  then x :: fun () -> filter p (k ())
  else                filter p (k ())

