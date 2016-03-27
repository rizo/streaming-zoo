open Elements
open Elements.Data

type ('a, 'r) step =
  'r -> 'a -> ('r, 'r) either

type ('a, 'b, 'r) transducer =
  ('b, 'r) step -> ('a, 'r) step

let map f =
  fun step -> fun r x ->
        step r (f x)

let filter p =
  fun step -> fun r x ->
        if p x then step r x else r

let take n =
  fun step ->
    let count = ref 0 in
    fun r x ->
      if !count < n
      then (incr count; step r x)
      else Left r

let compose t1 t2 =
  fun xf -> (t1 (t2 xf))

let (>>) = compose

let to_list xs x = xs @ [x]

(* let transduce_list_ t xs = *)
  (* List.fold ~f:(t conj_list) ~init:[] xs *)

let transduce_list ~into t xs =
  List.fold ~f:(t into) ~init:[] xs

let of_list xs xform =
  fun consumer -> List.fold ~f:(xform consumer) ~init:[] xs

let test () =
  let work = map ((+) 5) >> filter odd in
  let r = of_list [0; 1; 2; 3; 4] work to_list in
  assert (r = [5; 7; 9])

