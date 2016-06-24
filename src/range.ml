
module A = struct

  type ('a, 's) input_range = {
    init : 's;
    stop : 's -> bool;
    next : 's -> ('a * 's);
  }

  let count () =
    { init = 0;
      stop = (fun i -> false);
      next = (fun i -> i, i + 1) }

  let take n range =
    { init = (range.init, 0);
      stop = (fun (s, i) -> i = n || range.stop s);
      next = (fun (s, i) ->
          let (a, s') = range.next s in
          (a, (s', i + 1))) }

  let map f range =
    { range with next = (fun s -> let (a, s') = range.next s in (f a, s')) }

  let filter p range =
    { range with
      next = (fun s0 ->
          let rec loop s =
            let (a, s') = range.next s in
            if p a then (a, s')
            else loop s' in
          loop s0) }

  let fold f acc range =
    let rec loop acc s =
      if range.stop s then acc
      else
        let (a, s') = range.next s in
        loop (f acc a) s' in
    loop acc range.init
end

include A

