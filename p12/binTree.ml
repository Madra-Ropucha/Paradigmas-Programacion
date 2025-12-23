type 'a bin_tree =
    | Empty
    | Node of 'a bin_tree * 'a * 'a bin_tree;;

type 'a t = 'a bin_tree;;

let empty = Empty;;

let is_empty t = 
  match t with
    Empty -> true
  | _     -> false;;

let leaf_tree x = Node (Empty, x, Empty);;

let comb x l r = Node (l, x, r);;

let root t = 
    match t with 
      Empty -> raise (Failure "root")
    | Node (_, x, _) -> x;;

let left_branch t = 
    match t with 
      Empty -> raise (Failure "left_branch")
    | Node (l, _, _) -> l;;

let right_branch t = 
    match t with 
      Empty -> raise (Failure "right_branch")
    | Node (_, _, r) -> r;;

let rec size t = 
  match t with 
    Empty -> 0
  | Node (l, _, r) -> 1 + size l + size r;;

let rec height t = 
  match t with 
    Empty -> 0
  | Node (l, _, r) -> 1 + max (height l) (height r);;

let rec preorder t = 
    match t with
        Empty -> []
        | Node (l, x, r) -> [x] @ (preorder l) @ (preorder r);;

let rec inorder t = 
    match t with
        Empty -> []
        | Node (l, x, r) -> (inorder l) @ [x] @ (inorder r);;

let rec postorder t = 
    match t with
        Empty -> []
        | Node (l, x, r) -> (postorder l) @ (postorder r) @ [x]

let breadth t =
  let rec aux = function
    | [] -> []
    | Empty :: q -> aux q
    | Node (l, x, r) :: q -> x :: aux (q @ [l; r])
  in
  aux [t]

let rec leaves t = 
  match t with 
    Empty -> []
  | Node (Empty, x, Empty) -> [x]
  | Node (l, _, r) -> (leaves l) @ (leaves r);;

let rec find_in_depth func t =
  match t with
  | Empty -> raise (Not_found)
  | Node (l, y, r) ->
      if func y then y
      else
        try find_in_depth func l
        with Not_found -> find_in_depth func r;;

let rec exists p t =
    match t with 
        Empty -> false
        | Node (l, x, r) ->
            if p x then true
            else exists p l || exists p r;;

let rec for_all p t =
    match t with 
        Empty -> true
        | Node (l, x, r) ->
            if p x then for_all p l && for_all p r
            else false;;

let rec map f t = 
    match t with 
        Empty -> Empty
        | Node (l, x, r) -> Node (map f l, f x, map f r);;

let rec mirror t = 
    match t with 
        Empty -> Empty
        | Node (l, x, r) -> Node (mirror r, x, mirror l);;

let is_leave t =
  match t with
  Node (Empty, _, Empty) -> true
  | _ -> false;;