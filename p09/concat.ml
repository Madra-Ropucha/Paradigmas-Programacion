(*let concat l =
  let rec aux acc = function
    [] -> acc
    | h::t -> aux (List.append acc h) t
  in aux [] l*)
(*Por cada interacion, la funcion recorre acc y acc va creciendo conforme va pasando por cada elemento haciendo asi que sea muy lenta para listas grandes*)

(*let concat' l =
  List.fold_left (fun acc h -> List.append acc h) [] l*)
(*Por cada interacion, la funcion recorre acc y acc va creciendo conforme va pasando por cada elemento haciendo asi que sea muy lenta para listas grandes*)

let concat' l = 
  List.fold_right (fun h acc -> List.append h acc) l [];;
(* es mas eficiente porque fold_right solo recorre la lista 1 sola vez y h se concatena delante, evitando recorrer listas acumuladas largas *)

let concat'' l = 
  let rec aux acc = function
    []     -> acc
    | h::t -> match List.rev h with
                []     -> aux acc t
              | h'::t' -> aux (h'::acc) (t'::t)
  in List.rev (aux [] l);;
(* es mas eficiente ya que une el elemento al inicio de acc de manera terminal y al final se le da la vuelta para que la lista este en orden *)

let sublists l =
  List.fold_left (fun acc h ->
    acc @ (List.map (fun sub -> sub @ [h]) acc)
  ) [[]] l;;