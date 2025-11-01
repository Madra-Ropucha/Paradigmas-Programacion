if Array.length Sys.argv <> 2 then begin
  print_string "Invalid number of arguments\n";
  Sys.exit 1
end;

let rec fib n =
  if n <= 2 then 1
  else fib (n-1) + fib (n-2);;

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

let fastFib n =
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
;;