
type 'a t =
  | Nil
  | Cons of ('a * (unit -> 'a t))

let rec fold f acc l =
  match l with
  | Nil -> acc
  | Cons (x, k) -> fold f (f acc x) (k ())

let rec fold_until f acc l =
  match l with
  | Cons (a, k) ->
    begin match f acc a with
    | `Continue acc -> fold_until f acc (k ())
    | `Stop     acc -> acc
    end
  | Nil -> acc

let iota n =
  let rec loop acc count =
    if count = 0 then acc
    else loop (Cons (count, fun () -> acc)) (count - 1) in
  loop Nil n

let rev l =
  let rec loop acc l =
    match l with
    | Nil -> acc
    | Cons (x, k) -> loop (Cons (x, fun () -> acc)) (k ()) in
  loop Nil l

let map f l =
  rev (fold (fun acc x -> Cons (f x, fun () -> acc)) Nil l)

let filter_fold p l =
  rev (fold (fun acc x ->
      if p x then Cons (x, fun () -> acc)
      else acc) Nil l)

let take n l0 =
  let rec loop n l acc =
    if n = 0 then rev acc
    else match l with
      | Nil -> l0         (* n > length l0 => (take n l0 = l0), avoids rev *)
      | Cons (x, xs) ->
        loop (n - 1) xs (Cons (x, acc)) in
  loop n l0 Nil

(* let take_fold n l = *)
  (* rev (fst *)
         (* (fold_until *)
            (* (fun (acc, c) x -> *)
               (* if c = n *)
               (* then `Stop (acc, c) *)
               (* else `Continue (Cons (x, acc), c + 1)) *)
            (* (Nil, 0) l)) *)

