
(* Original *)
module A = struct
  type 'a t = ('a -> unit) -> unit

  let count n k =
    let count = ref 0 in
    while true do
      k !count;
      incr count
    done

  let rec init n f k =
    let i = ref 0 in
    while !i < n do
        k (f !i);
        incr i
    done

  let map f seq k =
    seq (fun x -> k (f x))

  let filter p seq k =
    seq (fun x -> if p x then k x)

  exception ExitTake

  let take n seq k =
    let count = ref 0 in
    try
      seq
        (fun x ->
           if !count = n then raise ExitTake;
           incr count;
           k x)
    with ExitTake -> ()

  exception Fold_stop_sequence

  let fold f acc seq =
    let r = ref acc in
    seq (fun elt -> r := f !r elt);
    !r

  let to_list seq =
    let r = ref [] in
    seq (fun elt -> r := elt :: !r);
    !r

  let rec of_list l k =
    match l with
    | [] -> ()
    | x :: xs ->
      k x; of_list xs k
end

(* Safe *)
module B = struct
  type 'a t  = ('a -> bool) -> unit

  let rec count n k =
    if k n then count (n + 1) k

  let rec of_list l (k: 'a -> bool) : unit =
    match l with
    | [] -> ()
    | x :: xs ->
      if k x
      then of_list xs k
      else ()

  let to_list (seq: ('a -> bool) -> unit) =
    let r = ref [] in
    seq (fun x -> r := x :: !r; true);
    List.rev !r

  let take n seq k =
    let count = ref 0 in
    seq (fun x ->
        if !count = n
        then false
        else let () = incr count in k x)

  let fold f acc seq =
    let r = ref acc in
    seq (fun elt -> r := f !r elt; true);
    !r

  let map f seq k =
    seq (fun x -> k (f x))

  let filter p seq k =
    seq (fun x -> if p x then k x else true)

end

(* Safe 2 *)
module C = struct
  type 'r reduced = Continue of 'r | Done of 'r

  let rec count n =
    fun step r ->
      match step r n with
      | Continue r' -> count (n + 1) step r'
      | Done r' -> r'

  let fold f acc seq =
    seq (fun r a -> Continue (f r a)) acc

  let map f seq =
    fun step ->
      seq (fun r a -> step r (f a))

  let filter p seq =
    fun step ->
      seq (fun r a -> if p a then step r a else Continue r)

  let take n seq =
    fun step ->
      let i = ref 0 in
      seq (fun r a ->
          if !i = n then Done r
          else (incr i; step r a))
end

(* Safe 3 *)
module D = struct
  let rec count n k =
    match k n with
    | `Continue -> count (n + 1) k
    | `Done -> ()

  let fold f acc seq =
    let r = ref acc in
    seq (fun a -> r := f !r a; `Continue);
    !r

  let map f seq k =
    seq (fun a -> k (f a))

  let filter p seq k =
    seq (fun a -> if p a then k a else `Continue)

  let take n seq k =
    let i = ref 0 in
    seq (fun a ->
        if !i = n then `Done
        else (incr i; k a))
end

(* Fast *)
module E = struct
  type 'a reduced = Continue of 'a | Stop of 'a

  type ('a, 'r) seq = ('r -> 'a -> 'r reduced) -> 'r -> 'r

  let rec count n : (int, 'r) seq =
    fun step init ->
      match step init n with
      | Continue r -> count (n + 1) step r
      | Stop r     -> r

  let take n seq =
    fun step init ->
      let count = ref 0 in
      seq (fun r a ->
          if !count = n
          then Stop r
          else (incr count; step r a)) init

  let fold f acc seq =
    seq (fun r a -> Continue (f r a)) acc

  let map f seq =
    fun step init ->
      seq (fun r a -> step r (f a)) init

  let filter p seq =
    fun step init ->
      seq (fun r a -> if p a then step r a else Continue r) init
end

(* Fast state *)
module F = struct
  type ('s, 'r) reduced = Continue of ('s * 'r) | Done of 'r

  type ('a, 's, 'r) reducer = 'r -> 'a -> ('s, 'r) reduced

  type ('a, 's, 'r) seq = ('a, 's, 'r) reducer -> 'r -> 'r

  let rec count n  =
    fun step s0 r0 ->
      match step s0 r0 n with
      | Continue (s, r) -> count (n + 1) step s r
      | Done r          -> r

  let take n seq =
    fun step s0 r0 ->
      let s0' = (s0, 0) in
      let step' (s, i) r a =
        if i = n then Done r
        else step (s, i + 1) r a in
      seq step' s0' r0

  let fold f r0 seq =
    let step s r a = Continue (s, f r a) in
    seq step () r0

  let map f seq =
    fun step s0 r0 ->
      let step' s r a = step s r (f a) in
      seq step' s0 r0

  let filter p seq =
    fun step s0 r0 ->
      let step' s r a =
        if p a then step s r a
        else Continue (s, r) in
      seq step' s0 r0
end

include A

