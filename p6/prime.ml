let is_prime n =
  let rec aux i =
    i >= n || (n mod i <> 0 && aux (i + 1))
  in n > 1 && aux 2

let is_prime2 n =
  if n <= 1 then false
  else if n = 2 || n = 3 then true
  else if n mod 2 = 0 then false
  else 
    let rec aux i =
      if i*i >= n then true
      else if n mod i = 0 then false
      else aux(i + 2)
    in aux 3

let goldbach n =
  if n <= 2 || n mod 2 <> 0 then failwith "goldbach: n tiene que ser par y > 2"
  else 
    let rec aux i =
      if is_prime2 i && is_prime2 (n - i) then (i, n - i)
      else aux (i + (if i mod 2 = 0 then 1 else 2))
    in aux 2 