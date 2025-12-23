let i_prod = List.fold_left (fun acc x -> acc * x) 1;;

let f_prod = List.fold_left (fun acc x -> acc *. x) 1.;;

let lmin l = 
  match l with
  | [] -> raise (Failure "lmin")
  | h :: t -> List.fold_left min h t;;

let lmin_lmax l =
  match l with 
  | []  -> raise (Failure "lmin_lmax")
  | h :: t -> (List.fold_left min h t, List.fold_left max h t);;

let last l = 
  match l with 
  | [] -> raise (Failure "last")
  | h :: t -> List.fold_left (fun _ x -> x) h t;;
let rev l = List.fold_left (fun acc x -> x :: acc) [] l;;

let rev_append l1 l2 = List.fold_left (fun acc x -> x :: acc) l2 l1;;

let rev_map f l = List.fold_left (fun acc x -> (f x) :: acc) [] l;;

let concat = List.fold_left (fun acc x -> acc ^ x) "";;
