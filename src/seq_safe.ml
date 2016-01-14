
let log = Printf.eprintf "%s\n"

type 'a t  = ('a -> bool) -> unit

let rec count n k =
  if k n then count (n + 1) k

let rec of_list l (k: 'a -> bool) : 'b =
  match l with
  | [] ->
    (* let () = log "of_list: producer is empty" in *)
    ()
  | x :: xs ->
    (* let () = log "of_list: will send value to consumer" in *)
    if k x then
      of_list xs k
    else
      (* let () = log "of_list: consumer is done" in *)
      ()

let to_list (seq: ('a -> bool) -> unit) =
  let r = ref [] in
  seq (fun x ->
      (* let () = log "to_list: got a value from producer" in *)
      r := x :: !r;
      true);
  List.rev !r

let take n seq k =
  let count = ref 0 in
  seq (fun x -> if !count = n
        then
          (* let () = log "take: processor is done" in *)
          false
        else
          let () = incr count in
          (* let () = log "take: redirect value to consumer" in *)
          k x)

let fold f acc seq =
  let r = ref acc in
  seq (fun elt -> r := f !r elt; true);
  !r

let map f seq k =
  seq (fun x -> k (f x))

let filter p seq k =
  seq (fun x -> if p x then k x else true)

