let hd l = 
  match l with
  | [] -> failwith "hd"
  | x :: _ -> x;;

let tl l = 
  match l with
  | [] -> failwith "tl"
  | _ :: tail -> tail;;

let rec last l = 
  match l with
  | [] -> failwith "last"
  | x :: [] -> x
  | _ :: tail -> last tail;;

let rec lenght l = 
    match l with
    | [] -> 0
    | _ :: tail -> 1 + lenght tail;;

let lenght' l = 
  let rec aux l counter = 
    match l with
    | [] -> counter
    | _ :: tail -> aux tail (counter + 1)
  in aux l 0;;
  
let rec compare_lengths l1 l2 = 
  match (l1, l2) with
  | ([], []) -> 0
  | ([], _ :: _) -> 1
  | (_ :: _, []) -> -1
  | (_ :: tail1, _ :: tail2) -> compare_lengths tail1 tail2;;

let rec append l1 l2 = 
  match l1 with
  | [] -> l2
  | x :: tail -> x :: (append tail l2);;

let rev_append l1 l2 = 
  let rec aux l1 l2 acc = 
    match l1 with
    | [] -> l2
    | x :: tail -> aux tail l2 (x :: acc)
  in aux l1 l2 [];;

let rev l = 
  let rec aux l acc = 
    match l with
    | [] -> acc
    | x :: tail -> aux tail (x :: acc)
  in aux l [];;

let rec concat ll = 
  match ll with
  | [] -> []
  | l :: tail -> append l (concat tail);;

let flatten ll = concat ll;;

let init l f = 
  let rec aux i acc =
    if i < 0 then rev acc
    else aux (i - 1) (f i :: acc)
  in aux (l - 1) [];;

let nth l n = 
  if n < 0 then raise (Failure "nth")
  else
  let rec aux l n =
    match l with
    | [] -> raise (Failure "nth")
    | x :: tail -> if n = 0 then x else aux tail (n - 1)
  in aux l n;;

let rec map f l = 
  match l with 
  | [] -> []
  | head :: tail -> f head :: map f tail;;

let rev_map f l = 
  let rec aux l acc = 
    match l with
    | [] -> acc
    | x :: tail -> aux tail (f x :: acc)
  in aux l [];;

let rec map2 f l1 l2 = 
  match (l1, l2) with
  | ([], []) -> []
  | (x1 :: tail1, x2 :: tail2) -> f x1 x2 :: map2 f tail1 tail2
  | _ -> raise (Failure "map2");;

let rec combine l1 l2 = 
  match (l1, l2) with
  | ([], []) -> []
  | (x1 :: tail1, x2 :: tail2) -> (x1, x2) :: combine tail1 tail2
  | _ -> raise (Failure "combine");;

let rec split l = 
  match l with
  | [] -> ([], [])
  | (x1, x2) :: tail -> 
      let (l1, l2) = split tail in
      (x1 :: l1, x2 :: l2);;

let find p l = 
  let rec aux l = 
    match l with
    | [] -> raise (Failure "find")
    | x :: tail -> if p x then x else aux tail
  in aux l;;

let rec filter p l = 
  match l with
  | [] -> []
  | x :: tail -> if p x then x :: filter p tail else filter p tail;;

let filter' p l = 
  let rec aux l acc = 
    match l with
    | [] -> rev acc
    | x :: tail -> if p x then aux tail (x :: acc) else aux tail acc
  in aux l [];;

let rec partition p l = 
  match l with
  | [] -> ([], [])
  | x :: tail -> 
      let (yes, no) = partition p tail in
      if p x then (x :: yes, no) else (yes, x :: no);;

let partition' p l = 
  let rec aux l (yes, no) = 
    match l with
    | [] -> (rev yes, rev no)
    | x :: tail -> 
        if p x then aux tail (x :: yes, no)
        else aux tail (yes, x :: no)
  in aux l ([], []);;

let for_all p l = 
  let rec aux l = 
    match l with
    | [] -> true
    | x :: tail -> p x && aux tail
  in aux l;;

let exists p l = 
  let rec aux l = 
    match l with
    | [] -> false
    | x :: tail -> p x || aux tail
  in aux l;;

let mem b l = 
  let rec aux l = 
    match l with
    | [] -> false
    | x :: tail -> x = b || aux tail
  in aux l;;

let rec take n l = 
  if n <= 0 then []
  else
    match l with
    | [] -> []
    | x :: tail -> x :: take (n - 1) tail;;

let drop n l = 
  let rec aux n l = 
    if n <= 0 then l
    else
      match l with
      | [] -> []
      | _ :: tail -> aux (n - 1) tail
  in aux n l;;

let fold_left f acc l = 
  let rec aux f acc l = 
    match l with
    | [] -> acc
    | x :: tail -> aux f (f acc x) tail
  in aux f acc l;;

let rec fold_right f l acc = 
  match l with
  | [] -> acc
  | x :: tail -> f x (fold_right f tail acc);;
