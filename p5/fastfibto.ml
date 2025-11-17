let fib n = let rec aux a b m = 
  if m = 0 then a
  else aux b (a + b) (m - 1)
in aux 0 1 n;;

let fibtoList n =
  let rec aux i =
    let fi = fib i in
    if fi > n then []
    else fi :: aux (i + 1)
  in
  aux 1

let rec printList l =
  match l with
  | [] -> ()
  | head :: [] -> 
    let _ = print_int head in
    print_newline ()
  | head :: tail ->
      let _ = print_int head in
      let _ = print_string ", " in
      printList tail  

let fibto n = 
  printList(fibtoList n)

let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "fibto: Invalid number of arguments";
    exit 1)
else
    let n = int_of_string Sys.argv.(1) in
    fibto n ;;