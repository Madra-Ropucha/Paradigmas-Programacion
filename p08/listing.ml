let from0to n = List.init (n + 1) (fun x -> x);;

let to0from n = List.init (n + 1) (fun x -> n - x);;

let pair x l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> aux ((x, h) :: acc) t
  in
  aux [] l;;  

let remove x l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> if h = x then List.rev_append acc t else aux (h :: acc) t
  in
  aux [] l;;

let remove_all x l = List.filter (fun y -> y <> x) l;;

let remove_duplicates l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> if List.mem h acc then aux acc t else aux (h :: acc) t
  in
  aux [] l;;

let ldif l1 l2 =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> if List.mem h l2 then aux acc t else aux (h :: acc) t
  in
  aux [] l1;;