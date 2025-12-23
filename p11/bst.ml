let is_bst ord t =
  let rec aux t min_val max_val =
    match t with
    | BinTree.Empty -> true
    | BinTree.Node (l, x, r) ->
        (* Comprueba que x respeta los límites *)
        let within_min =
          match min_val with
          | None -> true
          | Some v -> ord v x
        in
        let within_max =
          match max_val with
          | None -> true
          | Some v -> ord x v
        in
        within_min && within_max &&
        aux l min_val (Some x) &&
        aux r (Some x) max_val
  in
  aux t None None
