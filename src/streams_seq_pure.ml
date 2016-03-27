
let pass _ = failwith "pass"

type ('a, 'r) iter = ('r -> 'a option -> 'r) -> 'r -> 'r

let fold f state iter =
  iter f state

let map f iter step =
  iter (fun result item ->
      step result (f item))

let filter pred iter step =
  iter (fun result item ->
      if pred item then step result item
      else result)

(* let take n iter step = *)
(* let count = ref 0 in *)
(* iter (fun result input -> *)
(* if !count < n *)
(* then (incr count; step result input) *)
(* else result) *)

exception StopIteration

(* TODO: Add a struct with stop flag, and next fun *)

let take n iter step =
  let count = ref 0 in
  try iter (fun result item ->
      if !count < n then (incr count; step result input)
      else raise StopIteration)
  with StopIteration -> ()

let rec count n step =
  fun result -> count (n + 1) step (step result n)

let of_list l step result =
  List.fold_left ~f:(fun r x -> step r x) ~init:result l

let to_list iter =
  List.rev (iter (fun r x -> x :: r) [])

module A = struct
  type ('a, 'b) t =
    | Await of ('a -> ('a, 'b) t)
    | Done

  let rec of_list l iter =
    match iter, l with
    | Await step, x :: xs -> of_list xs (step x)
    | Await _, [] | Done, _ -> iter

  let rec to_list =
    let rec step acc x = step (x :: acc) in
    Await (step [])
end


