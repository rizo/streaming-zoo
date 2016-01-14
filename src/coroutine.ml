
type ('i, 'o, 'r) t =
  | Yield of ('o  * (unit -> ('i, 'o, 'r) t))
  | Await of ('i -> ('i, 'o, 'r) t)
  | Ready of 'r

let return r = Ready r

let rec (>>=) n f =
  match n with
  | Yield (b, n') -> Yield (b, fun () -> n' () >>= f)
  | Await k       -> Await (fun a -> k a >>= f)
  | Ready r       -> f r

let (>>) n1 n2 =
  n1 >>= fun _ -> n2

let empty   = Ready ()
let yield b = Yield (b, fun () -> Ready ())
let await   = Await (fun a -> Ready a)

let rec compose d u =
  match d, u with
  | Ready r       , _             -> Ready r
  | Yield (b, d') , _             -> Yield (b, fun () -> compose (d' ()) u)
  | Await k       , Yield (b, u') -> compose (k b) (u' ())
  | Await _       , Await k       -> Await (fun a -> compose d (k a))
  | Await _       , Ready r       -> Ready r

let (<-<) d u = compose d u
let (>->) u d = compose d u

let next n =
  match n with
  | Ready _       -> None
  | Yield (a, n') -> Some (a, n' ())
  | Await k       -> None

let rec iota n =
  let rec loop c =
    if c = n then Ready ()
    else Yield (n, fun () -> loop (c + 1)) in
  loop 0

let rec count n =
  Yield (n, fun () -> count (n + 1))

let rec map f =
  Await (fun a -> Yield (f a, fun () -> map f))

let rec filter f =
  Await (fun a ->
  if f a then Yield (a, fun () -> filter f)
  else filter f)

let rec take n =
  if n = 0 then Ready ()
  else Await (fun i -> Yield (i, fun () -> take (n - 1)))

let fold f state flow =
  let rec loop flow state =
    match next flow with
    | Some (a, rest) -> loop rest (f state a)
    | None           -> state in
  loop flow state


