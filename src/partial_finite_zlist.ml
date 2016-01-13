
open Elements

type 'a t = Nil | :: of 'a * 'a t lazy_t

let cons x xs = x :: lazy xs

(* Functor *)
let rec fmap f = function
  | x :: lazy xs -> f x :: lazy (fmap f xs)
  | Nil -> Nil

let head = function
  | x :: _ -> Some x
  | Nil -> None

let rec repeat x = x :: lazy (repeat x)

let tail = function
  | _ :: lazy xs -> Some xs
  | Nil -> None

let rec map f = function
  | x :: lazy xs -> f x :: lazy (map f xs)
  | Nil -> Nil

let rec zip_with f l1 l2 =
  match l1, l2 with
  | x :: lazy xs, y :: lazy ys -> f x y :: lazy (zip_with f xs ys)
  | _, _ -> fail "Lazy_list.zip_with: both lists must be non-empty"

(* Show *)
let rec show show_x (x :: _) =
  "Lazy_list.t " ^ show_x x ^ " :: <lazy>"

let rec intersperse y (x :: lazy xs) = x :: lazy (y :: lazy (intersperse y xs))

let rec interleave (x :: lazy xs) ys = x :: lazy (interleave ys xs)

let rec scan f z (x :: lazy xs) = z :: lazy (scan f (f z x) xs)

let scan1 f (x :: lazy xs) = scan f x xs

let rec transpose ((x :: lazy xs) :: lazy yss) =
    (x :: lazy (map head yss)) :: lazy (transpose (xs :: lazy (map tail yss)))

let rec iterate f x = x :: lazy (iterate f (f x))

let rec cycle l =
  let rec loop l' =
  if List.lenght l' = 0 then
    loop l
  else List.hd l' :: lazy (loop (List.tl l')) in
  loop l

let rec count n =
  n :: lazy (count (n + 1))

let rec unfold f c =
  let (x, d) = f c in
  x :: lazy (unfold f d)

let take n xs =
  if n < 0 then
    fail "Infinite_lazy_list.take: negative argument"
  else
    let rec loop n (x :: lazy xs)=
      if n = 0 then []
      else List.cons x (loop (n - 1) xs) in
    loop n xs

let drop n xs =
  if n < 0 then
    fail "Infinite_lazy_list.drop: negative argument"
  else
    let rec loop n xs =
      if n = 0 then xs
      else loop (n - 1) (tail xs) in
    loop n xs

let rec take_while p (x :: lazy xs) =
  if p x then List.cons x (take_while p xs)
  else []

let rec drop_while p (x :: lazy xs) =
  if p x then drop_while p xs
  else x :: lazy xs

let rec filter p (x :: lazy xs) =
  if p x then x :: lazy (filter p xs)
  else filter p xs

let rec partition p (x :: lazy xs) =
  let (trues, falses) = partition p xs in
  if p x then (x :: lazy trues, falses)
  else (trues, x :: lazy falses)

let rec span p (x :: lazy xs) =
  if p x then
    let (trues, falses) = span p xs in
    (List.cons x trues, falses)
  else
    ([], x :: lazy xs)

let rec group ?(eq = Pervasives.(=)) (x :: lazy ys) =
  let (xs, zs) = span (fun y -> x = y) ys in
  (List.cons x xs) :: lazy (group zs)

let nth n xs =
  if n < 0 then
    fail "Infinite_lazy_list.nth: negative argument"
  else
    let rec loop n (x :: lazy xs) =
      if n = 0 then x
      else loop (n - 1) xs in
    loop n xs

let rec zip (x :: lazy xs) (y :: lazy ys) = (x, y) :: lazy (zip xs ys)

let rec unzip ((x, y) :: lazy xys) =
  (x :: lazy (fst (unzip xys)), y :: lazy (snd (unzip xys)))

