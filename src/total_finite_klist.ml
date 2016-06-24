
type 'a t = unit -> 'a s
 and 'a s = (::) of 'a * 'a t | []

let rec count n () = n :: count (n + 1)

let rec init n f =
  let rec loop i () =
    if i = n then []
    else f i :: loop (i + 1) in
  loop 0

let rec fold f acc l =
  match l () with
  | [] -> acc
  | x :: k -> fold f (f acc x) k

let rec map f l () =
  match l () with
  | [] -> []
  | x :: k -> f x :: map f k

let rec take n l () =
  match l () with
  | [] -> []
  | x :: k ->
    if n = 0 then []
    else x :: (take (n - 1) k)

let rec of_list (l : 'a list) () =
  match l with
  | [] -> []
  | x :: xs -> x :: of_list xs

let to_list l =
  let rec loop acc l' =
    match l' () with
    | [] -> acc
    | x :: k -> loop (List.cons x acc) k in
  List.rev (loop [] l)

let rec filter p l () =
    match l () with
      | [] -> []
      | (x :: k) ->
        if p x
        then x :: filter p k
        else      filter p k ()


