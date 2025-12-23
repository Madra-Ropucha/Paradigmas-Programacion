let from0to n = List.init (n + 1) (fun x -> x);;

let to0from n = List.init (n + 1) (fun x -> n - x);;

let pair x l = List.map (fun y -> (x, y)) l;;

let remove x l =
  let rec aux = function
    | [] -> []
    | h :: t -> if h = x then t else h :: aux t
  in
  aux l;;

let remove_all x l = List.filter (fun y -> y <> x) l;;

let remove_duplicates l = List.fold_right (fun x acc -> if List.mem x acc then acc else x :: acc) l [];;

let ldif l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1;;

