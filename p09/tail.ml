let front l = 
  match l with 
    []   -> raise (Failure "front")
    | _  -> let rec aux acc = function
          h::[] -> acc
        | h::t  -> aux (h::acc) t
        | []    -> raise (Failure "front")
  in List.rev (aux [] l);;

let compress l = 
  match l with
      []     -> []
    | h::[]  -> [h]
    | h::t   -> let rec aux acc l = match l with
          []        -> acc
        | h::[]     -> h::acc
        | h1::h2::t -> if h1 == h2 then aux acc (h2::t)
                       else aux (h1::acc) (h2::t)
    in aux [] (List.rev l);;


let fold_right f l acc =
  let reverse = List.rev l
  in List.fold_left (fun aux x -> f x aux) acc reverse;;
(* TODO: ESTA MIERDA*)
