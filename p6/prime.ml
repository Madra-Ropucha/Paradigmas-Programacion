let is_prime n =
  let rec check_from i =
    i >= n || (n mod i <> 0 && check_from (i+1))
  in n > 1 && check_from 2

let is_prime2 n =
  if n <= 1 then false
  else if n = 2 || n = 3 then true
  else if n mod 2 = 0 then false
  else 
    let rec check_from i =
      if i*i >=n then true
      else if n mod i = 0 then false
      else check_from(i+2)
    in check_from 3

    let goldbach n =
      if n <= 2 || n mod 2 <> 0 then failwith "goldbach: n must be even and > 2"

      else 
        let rec check_gb i =
          if is_prime2 i && is_prime2 (n-i) then (i, n-i)
          else check_gb (i+(if i mod 2 = 0 then 1 else 2))
        in check_gb 2 