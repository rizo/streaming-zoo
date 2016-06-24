
module A = struct
  type 'a t = unit -> 'a option

  let count n =
    let i = ref 0 in
    fun () ->
      let x = !i in
      incr i;
      Some x

  let init n f =
    let i = ref 0 in
    fun () ->
      if !i = n then None
      else begin
        let x = !i in
        incr i;
        Some (f x)
      end

  let of_list l =
    let l = ref l in
    fun () ->
      match !l with
      | [] -> None
      | x::l' -> l := l'; Some x

  let map f gen =
    let stop = ref false in
    fun () ->
      if !stop then None
      else match gen () with
        | None -> stop := true; None
        | Some x -> Some (f x)

  let filter p gen =
    let rec next () =
      (* wrap exception into option, for next to be tailrec *)
      match gen () with
      | None -> None
      | (Some x) as res ->
        if p x
        then res      (* yield element *)
        else next ()  (* discard element *)
    in next

  let take n gen =
    let count = ref 0 in
    fun () ->
      if !count = n || !count = ~-1
      then None
      else match gen() with
        | None -> count := ~-1; None   (* indicate stop *)
        | (Some _) as x -> incr count; x

  let rec fold f acc gen =
    match gen () with
    | None -> acc
    | Some x -> fold f (f acc x) gen
end


(* Exn *)
module B = struct
  type 'a t = unit -> 'a

  let wrap f x = try Some (f x) with e -> None

  exception StopIteration

  let count n =
    let i = ref 0 in
    fun () ->
      let x = !i in
      incr i;
      x

  let of_list l =
    let l = ref l in
    fun () ->
      match !l with
      | [] -> raise StopIteration
      | x::l' -> l := l'; x

  let map f gen =
    fun () -> f (gen ())

  let filter p gen =
    let rec loop () =
      let x = gen () in
      if p x then x
      else loop () in
    loop

  let take n gen =
    let i = ref 0 in
    fun () ->
      if !i = n then raise StopIteration
      else (incr i; gen ())

  let rec fold f acc gen =
    match try Some (gen ()) with StopIteration -> None with
    | Some x -> fold f (f acc x) gen
    | None -> acc
end

include A

