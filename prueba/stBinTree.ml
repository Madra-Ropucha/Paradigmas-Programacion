type 'a st_bin_tree =
 | Leaf of 'a
 | Node of 'a st_bin_tree * 'a * 'a st_bin_tree

type 'a t = 'a st_bin_tree

let leaf_tree x = Leaf x

let is_leaf = function
 | Leaf _ -> true
 | _ -> false

let comb x l r = Node(l, x, r)

let root = function
 | Leaf x -> x
 | Node(_, x, _) -> x

let left_branch = function
 | Leaf _ -> raise (Failure "left_branch")
 | Node(l, _, _) -> l


let right_branch = function
 | Leaf x -> raise (Failure "right_branch")
 | Node(_, _, r) -> r


let rec size = function
 | Leaf _ -> 1
 | Node(l, x, r) -> 1 + size l + size r

  
let rec height = function
 | Leaf _ -> 1
 | Node(l, x, r) -> 1 + (max (height l) (height r))


let rec preorder = function
 | Leaf x -> [x]
 | Node(l, x, r) -> x::(preorder l) @ (preorder r)


let rec inorder = function
 | Leaf x -> [x]
 | Node(l, x, r) -> (inorder l) @ [x] @ (inorder r)


let rec postorder = function
 | Leaf x -> [x]
 | Node(l, x, r) -> (postorder l) @ (postorder r) @ [x]

let subtrees = function
    Leaf x -> []
  | Node (l, _, r) -> l::r::[]

let rec breadth_forest = function
  [] -> []
| l -> List.map root l @ breadth_forest (List.concat (List.map subtrees l))

let breadth t = breadth_forest [t]

let rec leaves = function
 | Leaf x -> [x]
 | Node(l, x, r) -> leaves l @ leaves r

let rec find_in_depth f = function
 | Leaf x -> if f x then x else raise Not_found
 | Node(l, x, r) -> if f x then x else try find_in_depth f l 
                                        with Not_found -> find_in_depth f r

let rec exists f = function
 | Leaf x -> f x
 | Node(l, x, r) -> f x || exists f l || exists f r

let rec for_all f = function
 | Leaf x -> f x
 | Node(l, x, r) -> f x && for_all f l && for_all f r

let rec map f = function
 | Leaf x -> Leaf (f x)
 | Node(l, x, r) -> Node(map f l, f x, map f r)

let rec mirror = function
 | Leaf x -> Leaf x
 | Node(l, x, r) -> Node(mirror r, x, mirror l)

let rec to_bin = function
 | Leaf x -> BinTree.leaf_tree x
 | Node(l, x, r) -> BinTree.comb x (to_bin l) (to_bin r)

let rec from_bin bt =
  if BinTree.is_empty bt then raise (Failure "from_bin")
   else try let l = BinTree.left_branch bt
            in let r = BinTree.right_branch bt 
               in if BinTree.is_empty l && BinTree.is_empty r then Leaf (BinTree.root bt)
                   else if BinTree.is_empty l || BinTree.is_empty r then raise (Failure "from_bin")
                         else Node(from_bin l, BinTree.root bt, from_bin r)
          with Not_found -> raise (Failure "from_bin")
