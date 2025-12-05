let i_prod = List.fold_left (fun acc x -> acc * x) 1;;

let false_prod = List.fold_left (fun acc x -> acc *. x) 1.;;

let lmin = List.fold_left (fun acc x -> if x < acc then x else acc) max_int;;

let lmin_lmax list = 
  match list with
  | [] -> failwith "lmin_lmax: empty list"
  | h :: t ->
      List.fold_left (fun (min_acc, max_acc) y ->
        if y < min_acc then (y, max_acc)
        else if y > max_acc then (min_acc, y)
        else (min_acc, max_acc)) (min_int, max_int) t

let last = List.fold_left (fun _ x -> x);;

let rev = List.fold_left (fun acc x -> x :: acc) [];;

let rev_append l1 l2 = List.fold_left (fun acc x -> x :: acc) l2 l1;;

let rev_map f l = List.fold_left (fun acc x -> (f x) :: acc) [] l;;

let concat = List.fold_left (fun acc x -> acc ^ x) "";;
