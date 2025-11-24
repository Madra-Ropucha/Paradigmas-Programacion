let front l = 
  match l with 
    []   -> raise (Failure "front")
    | _  -> let rec aux acc = function
          h::[] -> acc
        | h::t  -> aux (h::acc) t
        | []    -> raise (Failure "front")
  in List.rev (aux [] l);;

let compress l = 

let fold_right f l acc =
