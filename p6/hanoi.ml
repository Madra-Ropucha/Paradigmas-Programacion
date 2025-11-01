let otro origen destino = 
  6 - origen - destino;;

let mueve (origen, destino) = 
  match (origen, destino) with
  | (1, 2) -> "1-->2   3\n"
  | (1, 3) -> "1---2-->3\n"
  | (2, 3) -> "1   2-->3\n"
  | (2, 1) -> "1<--2   3\n"
  | (3, 1) -> "1<--2---3\n"
  | (3, 2) -> "1   2<--3\n"
  | (o, d) -> failwith (Printf.sprintf "Invalid move: (%d, %d)" o d)
;;

let rec hanoi n origen destino =
  (* n número de print_hanoi 3 3 2;;discos, 1 <= origen <= 3, 1 <= destinot <= 3, origen <> destino *)
  if n = 0 then ""
  else
    let otro = otro origen destino in
    hanoi (n-1) origen otro ^ mueve (origen, destino) ^ hanoi (n-1) otro destino;;

let rec hanoi n origen destino =
  (* n número de print_hanoi 3 3 2;;discos, 1 <= origen <= 3, 1 <= destinot <= 3, origen <> destino *)
  if n = 0 then ""
  else
    let otro = otro origen destino in
    hanoi (n-1) origen otro ^ mueve (origen, destino) ^ hanoi (n-1) otro destino;;
    
let hanoi n origen destino =
  if n = 0 || origen = destino then ""
  else hanoi n origen destino;;

let print_hanoi n origen destino =
  if n < 0 || origen < 1 || origen > 3 || destino < 1 || destino > 3
  then print_endline "**ERROR**"
  else print_endline ("=========\n" ^
                      hanoi n origen destino ^
                      "=========");;

let crono f x =
  let t = Sys.time () in
  f x; Sys.time () -. t;;