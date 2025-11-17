let otro origen destino =
6 - origen - destino

let mueve (origen, destino) =
  match (origen, destino) with
  | (1, 2) -> "1---2-->3\n"
  | (1, 3) -> "1---2-->3\n"
  | (2, 1) -> "1   2<--3\n"
  | (2, 3) -> "1-->2   3\n"
  | (3, 1) -> "1<--2---3\n"
  | (3, 2) -> "1   2<--3\n"
  | _ -> ""

let rec hanoi n origen destino =
  (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
  if n = 0 then ""
  else
    let otro = otro origen destino in
    hanoi (n-1) origen otro ^ mueve (origen, destino) ^ hanoi (n-1) otro destino

let hanoi n origen destino =
  if n = 0 || origen = destino then ""
  else hanoi n origen destino

let print_hanoi n origen destino =
  if n < 0 || origen < 1 || origen > 3 || destino < 1 || destino > 3
  then print_endline "**ERROR**"
  else print_endline ("=========\n" ^
                      hanoi n origen destino ^
                      "=========")

let crono f x =
  let t = Sys.time () in
  f x; Sys.time () -. t;;

let rec n_hanoi_mov n nd origen destino  = 
let otro = otro origen destino in 
  if nd = 1  
  then (origen, destino) 
  else  let mitad = int_of_float (2. ** float (nd - 1)) in
  if n < mitad 
    then n_hanoi_mov n (nd - 1) origen otro  
  else if n = mitad  
    then (origen, destino) 
  else n_hanoi_mov (n - mitad) (nd - 1) otro destino