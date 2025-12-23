

exception Not_found

type move = North | South | West | East
type position = int*int
type robot = position * move

let move (x,y) = function
| North -> (x-1,y)
| South -> (x+1,y)
| East -> (x, y+1)
| West -> (x, y-1)

let inside (m,n) (x,y) =
x>= 1 && x<= m && y>= 1 && y <= n

(* mover un solo robot *)
let robot_move (m,n) ((x,y), dir) =
  match dir with
  | North ->
      if x = 1 then ((2,y), South)
      else ((x-1, y), North)
  | South ->
      if x = m then ((m-1,y), North)
      else ((x+1, y), South)
  | West ->
      if y = 1 then ((x,2), East)
      else ((x, y-1), West)
  | East ->
      if y = n then ((x,n-1), West)
      else ((x, y+1), East)

(* mover todos los robots *)
let robot_moves dims robots =
  List.map (robot_move dims) robots

let rec robots_at dims robots t =
  if t = 0 then robots
  else robots_at dims (robot_moves dims robots) (t-1)


let robot_positions dims robots t =
  robots_at dims robots t
  |> List.map fst


let tour m n robots = 

    let dims = (m,n) in 
    let goal = (m,n) in

    let rec dfs pos time visited = 
        if pos = goal then []
        else 
            let next_time = time + 1 in
            let robot_pos = robot_positions dims robots next_time in
            let try_move mv =
  	      let new_pos = move pos mv in
              if inside dims new_pos
     		&& not (List.mem new_pos visited)
    		&& not (List.mem new_pos robot_pos)
              then
    		mv :: dfs new_pos next_time (new_pos :: visited)
 	      else
    		raise Not_found
	   in
	   try
             try_move North
	   with Not_found ->
           try
             try_move South
           with Not_found ->
           try
             try_move East
          with Not_found ->
            try_move West

        in

    dfs (1,1) 0 [(1,1)]
