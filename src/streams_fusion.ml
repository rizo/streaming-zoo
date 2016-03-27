
type ('a, 's) step =
  | Done
  | Skip  of 's
  | Yield of 'a * 's

type +_ t =
    Stream : 's * ('s -> ('a, 's) step) -> 'a t

let fold f init (Stream (seed, next)) =
  let rec loop seed v next f =
    match next seed with
    | Done         -> v
    | Skip  s      -> loop s v next f
    | Yield (a, s) -> loop s (f v a) next f in
  loop seed init next f

let unfold f init =
  let next s =
    match f s with
    | None -> Done
    | Some (a, s) -> Yield (a, s) in
  Stream (init, next)

let of_list l =
  let next s =
    match s with
    | [] -> Done
    | x :: xs -> Yield (x, xs) in
  Stream (l, next)

let map f (Stream (seed, next)) =
  let next s =
    match next s with
    | Done         -> Done
    | Skip s       -> Skip s
    | Yield (a, s) -> Yield (f a, s) in
  Stream (seed, next)

let filter p (Stream (seed, next)) =
  let next s =
    match next s with
    | Done         -> Done
    | Skip s       -> Skip s
    | Yield (a, s) when p a -> Yield (a, s)
    | Yield (_, s) -> Skip s in
  Stream (seed, next)

let take n (Stream (s, next)) =
  if n < 0 then invalid_arg "take";
  let next (i, s) =
    if i >= n then Done
    else
      match next s with
      | Done         -> Done
      | Skip s       -> Skip (i, s)
      | Yield (a, s) -> Yield (a, (i + 1, s)) in
  Stream ((0, s), next)

let count n =
  Stream (n, fun n -> Yield (n, n + 1))

let bench () =
  let r = count 0
          |> map    (fun x -> x + 1)
          |> map    (fun x -> x + 1)
          |> map    (fun x -> x + 1)
          |> map    (fun x -> x + 1)
          |> filter (fun x -> x mod 3 == 0)
          |> map    (fun x -> x + 5)
          |> filter (fun x -> x mod 5 == 0)
          |> map    (fun x -> x + 50)
          |> take 3000000 in
  print_endline (string_of_int (fold (+) 0 r))

