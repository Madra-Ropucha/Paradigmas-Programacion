type 'a st_bin_tree =
    | Leaf of 'a
    | Node of 'a st_bin_tree * 'a * 'a st_bin_tree;;

type 'a t = 'a st_bin_tree;;

let leaf_tree x = Leaf x;;

let is_leaf t = 
  match t with
    Leaf _ -> true
  | _      -> false;;

let comb x l r = Node (l, x, r);;

let root t = 
    match t with 
      Leaf x -> x
    | Node (_, x, _) -> x;;

let left_branch t = 
    match t with 
      Leaf _ -> raise (Failure "left_branch")
    | Node (l, _, _) -> l;;

let right_branch t = 
    match t with 
      Leaf _ -> raise (Failure "right_branch")
    | Node (_, _, r) -> r;;

let rec size t = 
    match t with
        Leaf _ -> 1
        | Node (l, _, r) -> 1 + size l + size r;;

let rec height t =
    match t with
        Leaf _ -> 1
        | Node (l, _, r) -> 1 + max (height l) (height r);;

let rec preorder t = 
    match t with
        Leaf x -> [x]
        | Node (l, x, r) -> x :: (preorder l) @ (preorder r);;

let rec inorder t = 
    match t with
        Leaf x -> [x]
        | Node (l, x, r) -> (inorder l) @ [x] @ (inorder r);;

let rec postorder t = 
    match t with
        Leaf x -> [x]
        | Node (l, x, r) -> (postorder l) @ (postorder r) @ [x];;

let breadth t = 
    let rec aux queue =
        match queue with
        | [] -> []
        | Leaf x :: rest -> x :: aux rest
        | Node (l, x, r) :: rest ->
            x :: aux (rest @ [l; r])
    in
    aux [t];;

let rec leaves t = 
    match t with 
        Leaf x -> [x]
        | Node (l, _, r) -> (leaves l) @ (leaves r);;

let find_in_depth func t = 
    let rec aux t = 
        match t with
        | Leaf x -> if func x then x else raise Not_found
        | Node (l, x, r) ->
            if func x then x
            else
                try aux l
                with Not_found -> aux r
            in aux t;;

let rec exists func t = 
    match t with
        Leaf x -> func x
        | Node (l, x, r) -> func x || exists func l || exists func r;;

let rec for_all p t = 
    match t with 
        Leaf x -> p x
        | Node (l, x, r) -> p x && for_all p l && for_all p r;;

let rec map f t = 
    match t with
        Leaf x -> Leaf (f x)
        | Node (l, x, r) -> Node (map f l, f x, map f r);;

let rec mirror t = 
    match t with 
        Leaf x -> Leaf x
        | Node (l, x, r) -> Node (mirror r, x, mirror l);;

let rec to_bin t =
    match t with
  | Leaf x -> BinTree.leaf_tree x
  | Node (l, x, r) -> BinTree.comb x (to_bin l) (to_bin r);;

let rec from_bin t =
    if BinTree.is_empty t then raise (Failure "from_bin")
    else if BinTree.is_empty(BinTree.left_branch t) && BinTree.is_empty(BinTree.right_branch t) then
        Leaf (BinTree.root t)
    else
        Node (from_bin (BinTree.left_branch t),
              BinTree.root t,
              from_bin (BinTree.right_branch t));;