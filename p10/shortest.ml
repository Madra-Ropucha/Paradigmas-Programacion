exception Not_found

type move = North | South | West | East
type position = int * int
type robot = position * move


let move_pos (x,y) = function
  | North -> (x-1, y)
  | South -> (x+1, y)
  | East  -> (x, y+1)
  | West  -> (x, y-1)

let inside (m,n) (x,y) =
  x >= 1 && x <= m && y >= 1 && y <= n

let move_robot (m,n) ((x,y), dir) =
  match dir with
  | North ->
      if x = 1 then ((2,y), South) else ((x-1,y), North)
  | South ->
      if x = m then ((m-1,y), North) else ((x+1,y), South)
  | West ->
      if y = 1 then ((x,2), East) else ((x,y-1), West)
  | East ->
      if y = n then ((x,n-1), West) else ((x,y+1), East)

let move_robots dims robots =
  List.map (move_robot dims) robots

let rec robots_at dims robots t =
  if t = 0 then robots
  else robots_at dims (move_robots dims robots) (t-1)

let robot_positions dims robots t =
  robots_at dims robots t |> List.map fst


let shortest m n robots =
  let dims = (m,n) in
  let goal = (m,n) in

  let rec reach_shortest queue visited =
    match queue with
    | [] -> raise Not_found
    | (pos, time, path) :: rest ->
      if pos = goal then List.rev path
      else
        let next_time = time + 1 in
        let robot_pos = robot_positions dims robots next_time in

        let expand mv =
          let new_pos = move_pos pos mv in
          inside dims new_pos &&
          not (List.mem new_pos visited) &&
          not (List.mem new_pos robot_pos)
        in

        let next =
          [North; South; East; West]
          |> List.filter expand
          |> List.map (fun mv ->
          (move_pos pos mv, next_time, mv :: path))
        in

        reach_shortest 
          (rest @ next)
          (List.map (fun (p,_,_) -> p) next @ visited) 
  in
  reach_shortest [(1,1), 0, []] [(1,1)]