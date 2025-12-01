let front l =
  let rec aux acc = function
    | [] | _::[] -> List.rev acc
    | h::t       -> aux (h::acc) t
  in aux [] l;;

let compress l =
  let rec aux acc = function
    | []        -> List.rev acc
    | [h]       -> List.rev (h::acc)
    | h1::h2::t ->
        if h1 = h2 then aux acc (h2::t)
        else aux (h1::acc) (h2::t)
  in aux [] l;;
