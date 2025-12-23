type 'a g_tree = 
    Gt of 'a * 'a g_tree list;;

type 'a t = 'a g_tree;;

let leaf_tree x = Gt (x, []);;

let root (Gt (x, _)) = x;;

let branches (Gt (_, bs)) = bs;;

let size t = 
    let rec aux (Gt (_, bs)) =
        1 + List.fold_left (fun acc b -> acc + aux b) 0 bs
    in aux t;;

let height t = 
    let rec aux (Gt (_, bs)) =
        match bs with
        | [] -> 1
        | _  -> 1 + List.fold_left (fun acc b -> max acc (aux b)) 0 bs
    in aux t;;

let preorder t = 
    let rec aux (Gt (x, bs)) =
        x :: List.flatten (List.map aux bs)
    in aux t;;

let postorder t = 
    let rec aux (Gt (x, bs)) =
        List.flatten (List.map aux bs) @ [x]
    in aux t;;

let breadth t = 
    let rec aux queue =
        match queue with
        | [] -> []
        | Gt (x, bs) :: rest ->
            x :: aux (rest @ bs)
    in
    aux [t];; 

let rec leaves t = 
    match t with
        Gt (x, []) -> [x]
        | Gt (_, bs) -> List.flatten (List.map leaves bs);;

let find_in_depth func t = 
    let rec aux t = 
        match t with
        | Gt (x, bs) ->
            if func x then x
            else
                let rec try_branches = function
                    | [] -> raise Not_found
                    | b :: rest ->
                        try aux b
                        with Not_found -> try_branches rest
                in try_branches bs
    in aux t;;  

let breadth_find func t = 
    let rec aux queue =
        match queue with
        | [] -> raise Not_found
        | Gt (x, bs) :: rest ->
            if func x then x
            else aux (rest @ bs)
    in aux [t];;

let exists func t = 
    try 
        let _ = find_in_depth func t in true
    with Not_found -> false;;

let rec for_all func t = 
    match t with
      Gt(x, bs) ->
        if func x then
            List.for_all (for_all func) bs
        else false;;

let rec map f t = 
    match t with
        Gt (x, bs) -> Gt (f x, List.map (map f) bs);;

let rec mirror t = 
    match t with 
        Gt (x, bs) -> Gt (x, List.rev (List.map mirror bs));;

let rec from_bin t =
    if BinTree.is_empty(BinTree.left_branch t) && BinTree.is_empty(BinTree.right_branch t) then
        let x = BinTree.root t in
        Gt (x, [])
    else
        let x = BinTree.root t in
        let l = BinTree.left_branch t in
        let r = BinTree.right_branch t in
        Gt (x, [from_bin l; from_bin r]);;

let rec from_st_bin t = 
    let rec aux t =
        match t with
        | StBinTree.Leaf x -> Gt (x, [])
        | StBinTree.Node (l, x, r) ->
            Gt (x, [aux l; aux r])
    in aux t;;