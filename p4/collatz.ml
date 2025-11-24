let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec check n = n = 1 || check (f n)

let rec check_to n =
  if n = 1 then check 1
  else check n && check_to (n - 1)

let rec orbit n =
  if n = 1 then "1"
  else
    let next = f n in
    string_of_int n ^ ", " ^ orbit next

let rec length n =
  if n = 1 then 1
  else 1 + length (f n)

let rec top n =
  if n = 1 then 1
  else
    let next = f n in
    let t = top next in
    if n > t then n else t
