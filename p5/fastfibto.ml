let fib n = let rec aux a b m = 
  if m = 0 then a
  else aux b (a + b) (m - 1)
in aux 0 1 n;;

let fibto n =
  let rec f x =
    if fib x > n || fib x < 1 then ()
    else begin
      print_int (fib x);
      if fib(x+1) < n then print_string ", ";
      f (x + 1)
    end
  in
  f 1;;

let rec fibtoList l = 
  let local = 1 in
    match (local, l) with
    | fib(local) < l -> fib(local) :: 
    | fib(local) > l -> 

let rec printList l =
  match l with
  | [] -> ()
  | [head] -> print_int head
  | head :: tail ->
      let _ = print_int head in
      let _ = print_string ", " in
      printList tail

let fibto lista = 
  printList()

let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "fibto: Invalid number of arguments";
    exit 1
  );