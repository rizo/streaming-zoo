
let log = Printf.eprintf "%s\n"

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

