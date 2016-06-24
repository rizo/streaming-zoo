
type 'a t = [] | (::) of 'a * 'a t lazy_t

let cons x xs = x :: lazy xs

let head = function
  | x :: _ -> Some x
  | [] -> None

let rec init n f =
  let rec loop i =
    if i = n then []
    else f i :: lazy (loop (i + 1)) in
  loop 0

let rec repeat x = x :: lazy (repeat x)

let tail = function
  | _ :: lazy xs -> Some xs
  | [] -> None

let rec fold f z = function
  | [] -> z
  | x :: lazy xs -> fold f (f z x) xs

let rec map f = function
  | x :: lazy xs -> f x :: lazy (map f xs)
  | [] -> []

let rec zip_with f l1 l2 =
  match l1, l2 with
  | x :: lazy xs, y :: lazy ys -> f x y :: lazy (zip_with f xs ys)
  | _, _ -> failwith "Lazy_list.zip_with: both lists must be non-empty"

let rec intersperse y = function
  | (x :: lazy xs) -> x :: lazy (y :: lazy (intersperse y xs))
  | [] -> []

let rec interleave = function
  | (x :: lazy xs) ->fun ys -> x :: lazy (interleave ys xs)
  | [] -> fun ys -> []

let rec scan f z = function
  | (x :: lazy xs) -> z :: lazy (scan f (f z x) xs)
  | [] -> []

let reduce f = function
  | (x :: lazy xs) -> scan f x xs
  | [] -> []

let rec iterate f x = x :: lazy (iterate f (f x))

let rec cycle l =
  let rec loop l' =
    if List.length l' = 0 then
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
    failwith "Infinite_lazy_list.take: negative argument"
  else
    let rec loop n = function
      | (x :: lazy xs) ->
        if n = 0 then []
        else x :: lazy (loop (n - 1) xs)
      | [] -> [] in
    loop n xs

let drop n xs =
  if n < 0 then
    failwith "Infinite_lazy_list.drop: negative argument"
  else
    let rec loop n xs =
      if n = 0 then xs
      else match tail xs with
        | None -> []
        | Some xs' -> loop (n - 1) xs' in
    loop n xs

let rec take_while p = function
  | (x :: lazy xs) ->
    if p x then x :: lazy (take_while p xs)
    else []
  | [] -> []

let rec drop_while p = function
  | (x :: lazy xs) ->
    if p x then drop_while p xs
    else x :: lazy xs
  | [] -> []

let rec filter p = function
  | (x :: lazy xs) ->
    if p x then x :: lazy (filter p xs)
    else filter p xs
  | [] -> []

let rec partition p = function
  | (x :: lazy xs) ->
    let (trues, falses) = partition p xs in
    if p x then (x :: lazy trues, falses)
    else (trues, x :: lazy falses)
  | [] -> [], []

let rec span p = function
  | (x :: lazy xs) ->
    if p x then
      let (trues, falses) = span p xs in
      (List.cons x trues, falses)
    else
      ([], x :: lazy xs)
  | [] -> [], []

let rec group ?(eq = Pervasives.(=)) = function
  | (x :: lazy ys) ->
    let (xs, zs) = span (fun y -> x = y) ys in
    (List.cons x xs) :: lazy (group zs)
  | [] -> []

let nth n xs =
  if n < 0 then
    failwith "Infinite_lazy_list.nth: negative argument"
  else
    let rec loop n = function
      | (x :: lazy xs) ->
        if n = 0 then Some x
        else loop (n - 1) xs
      | [] -> None in
    loop n xs

let rec zip xs ys =
  match xs, ys with
  | (x :: lazy xs'), (y :: lazy ys') -> (x, y) :: lazy (zip xs' ys')
  | [], [] -> []
  | _ -> failwith "zip: the lists should have the same length"

let rec unzip = function
  | ((x, y) :: lazy xys) ->
    (x :: lazy (fst (unzip xys)), y :: lazy (snd (unzip xys)))
  | [] -> [], []


