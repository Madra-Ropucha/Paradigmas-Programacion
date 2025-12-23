type 'a a_tree = 'a option array
type 'a t = 'a a_tree

let from_bin (t : 'a BinTree.t) : 'a t =
  let h = BinTree.height t in
  let size = (1 lsl h) - 1 in
  let a = Array.make size None in

  let rec fill i t =
    if i < size && not (BinTree.is_empty t) then (
      a.(i) <- Some (BinTree.root t);
      fill (2*i + 1) (BinTree.left_branch t);
      fill (2*i + 2) (BinTree.right_branch t)
    )
  in
  fill 0 t;
  a

let breadth (a : 'a t) : 'a list =
  let res = ref [] in
  for i = 0 to Array.length a - 1 do
    match a.(i) with
    | Some x -> res := !res @ [x]
    | None -> ()
  done;
  !res

let mem (cmp : 'a -> 'a -> bool) (x : 'a) (a : 'a t) : bool =
  let i = ref 0 in
  let found = ref false in

  while !i < Array.length a && not !found do
    match a.(!i) with
    | None ->
        i := Array.length a
    | Some y ->
        if not (cmp x y) && not (cmp y x) then
          found := true
        else if cmp x y then
          i := 2 * !i + 1
        else
          i := 2 * !i + 2
  done;
  !found

