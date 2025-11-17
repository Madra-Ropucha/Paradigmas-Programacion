let rec fib n =
  if n <= 2 then 1
  else fib (n-1) + fib (n-2);;

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

(*let fastFib n =
  let x = ref 0 in 
  let y = ref 1 in
  let counter = ref n in
  while !counter > 0 do
    if !x > !y then
      y := !y + !x
    else 
      x := !x + !y;
    counter := !counter - 1
  done;
  if !x > !y then !x else !y
;;*)