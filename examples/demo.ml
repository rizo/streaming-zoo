
open Elements

let show_list show_item l =
  fmt "[%s]" @@ String.concat ", " (List.map ~f:show_item l)

let izl_demo () =
  let open Infinite_lazy_list in
  let l1 = take 6 (map (fun x -> if x = 0 then false else true) (cycle [0; 1])) in
  print @@ show_list (function true -> "T" | false -> "F") l1;

  let l2 = take 10 @@ zip_with (+) (count 0) (count 1) in
  print @@ show_list Int.to_string l2

let () =
  izl_demo ()

