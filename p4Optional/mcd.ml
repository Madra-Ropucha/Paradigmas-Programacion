let rec mcd (x, y) =
  if x = 0 then y
  else if y = 0 then x
  else if x > y then mcd (x - y, y)
  else mcd (x, y - x)

let rec mcd' (x, y) =
  if x = 0 then y
  else if y = 0 then x
  else if x > y then mcd' (x mod y, y)
  else mcd' (x, y mod x)
