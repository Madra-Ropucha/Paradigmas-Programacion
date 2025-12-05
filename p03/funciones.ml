let rec sum_to = fun n -> n + if n <= 0 then 0 else sum_to(n - 1)

let rec exp_2 = fun n ->  if n <= 0 then 1 else 2 * exp_2(n - 1)

let rec num_cifras = fun n -> if abs(n) < 10 then 1 else 1 + num_cifras(n / 10)

let rec sum_cifras = fun n -> if n == min_int then 85 else let y = if n < 0 then -n else n in if y < 10 then y else y mod 10 + sum_cifras(y / 10)