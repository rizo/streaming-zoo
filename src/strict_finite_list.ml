
type 'a t =
  | Nil
  | Cons of ('a * 'a t)

(* Fold & Fold Until *)

let rec fold f acc l =
  match l with
  | Nil -> acc
  | Cons (x, xs) -> fold f (f acc x) xs

let rec fold_until f acc l =
  match l with
  | Cons (a, rest) ->
    begin match f acc a with
    | `Continue acc -> fold_until f acc rest
    | `Stop     acc -> acc
    end
  | Nil -> acc

(* Iota *)

let iota n =
  let rec loop acc count =
    if count = 0 then acc
    else loop (Cons (count, acc)) (count - 1) in
  loop Nil n

(* Rev *)

let rev l =
  let rec loop acc l =
    match l with
    | Nil -> acc
    | Cons (x, xs) -> loop (Cons (x, acc)) xs in
  loop Nil l

(* Map *)

let rec map_no_tco f l =
  match l with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map_no_tco f xs)

let map_tco f l =
  let rec loop acc l =
    match l with
    | Nil -> rev acc
    | Cons (x, xs) -> loop (Cons (f x, acc)) xs in
  loop Nil l

let map_fold f l =
  rev (fold (fun acc x -> Cons (f x, acc)) Nil l)

let map = map_fold

(* Filter *)

let filter_tco p l =
  let rec loop acc l =
    match l with
    | Nil -> rev acc
    | Cons (x, xs) when p x -> loop (Cons (x, acc)) xs
    | Cons (_, xs) -> loop acc xs in
  loop Nil l

let filter_fold p l =
  rev (fold (fun acc x ->
      if p x then Cons (x, acc)
      else acc) Nil l)

let filter = filter_fold

(* Take *)

let take n l0 =
  let rec loop n l acc =
    if n = 0 then rev acc
    else match l with
      | Nil -> l0         (* n > length l0 => (take n l0 = l0), avoids rev *)
      | Cons (x, xs) ->
        loop (n - 1) xs (Cons (x, acc)) in
  loop n l0 Nil

let take_fold n l =
  rev (fst
         (fold_until
            (fun (acc, c) x ->
               if c = n
               then `Stop (acc, c)
               else `Continue (Cons (x, acc), c + 1))
            (Nil, 0) l))

