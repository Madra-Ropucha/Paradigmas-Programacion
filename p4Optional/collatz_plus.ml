let f n =
  if n mod 2 = 0 then n / 2 else 3 * n + 1

let rec length_and_top n =
  if n = 1 then
    (1, 1)
  else
    let next = f n in
    let (len_tail, top_tail) = length_and_top next in
    let max_here = if n > top_tail then n else top_tail in
    (len_tail + 1, max_here)
