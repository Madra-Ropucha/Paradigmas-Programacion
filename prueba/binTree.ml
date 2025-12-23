type 'a bin_tree =
  Empty | Node of 'a bin_tree * 'a * 'a bin_tree

type 'a t = 'a bin_tree

let empty = Empty

let is_empty t = (t = empty)

let leaf_tree n = Node(Empty, n, Empty)

let comb x l r = Node(l, x, r)

let root = function
 | Empty -> raise (Failure "root")
 | Node(_, x, _) -> x

let left_branch = function
 | Empty -> raise (Failure "left_branch")
 | Node(l, _, _) -> l

let right_branch = function 
 | Empty -> raise (Failure "right_branch")
 | Node(_, _, r) -> r

let rec size = function
 | Empty -> 0
 | Node(l, _, r) -> 1 + size l + size r

let rec height = function
 | Empty -> 0
 | Node(l, _, r) -> 1 + max (height l) (height r)

let rec preorder = function
 | Empty -> []
 | Node(l, x, r) -> x::(preorder l) @ (preorder r)

let rec inorder = function
 | Empty -> []
 | Node(l, x, r) -> (inorder l) @ [x] @ (inorder r)

let rec postorder = function
 | Empty -> []
 | Node(l, x, r) -> (postorder l) @ (postorder l) @ [x]

let rec breadth = function
 | Empty -> []
 | Node(l, x, r) -> [x] @ (breadth l) @ (breadth r)

let rec leaves t = 
  match t with 
   | Empty -> []
   | Node(Empty, x, Empty) -> [x]
   | Node(l, _, r) -> (leaves l) @ (leaves r)

let rec find_in_depth f = function
 | Empty -> raise (Not_found)
 | Node(l, x, r) -> if f x then x
                     else try find_in_depth f l 
                           with Not_found -> find_in_depth f r

let rec exists f = function
 | Empty -> false
 | Node(l, x, r) -> f x || exists f l || exists f r

let rec for_all f = function 
 | Empty -> true
 | Node(l, x, r) -> f x && for_all f l && for_all f r

let rec map f = function
 | Empty -> Empty
 | Node(l, x, r) -> Node(map f l, f x, map f r)

let rec mirror = function 
 | Empty -> Empty
 | Node(l, x, r) -> Node(mirror r, x, mirror l)
