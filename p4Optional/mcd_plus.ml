let rec mcd_pasos (x, y) =
  if x = 0 then (y, 1)
  else if y = 0 then (x, 1)
  else if x > y then
    let (d, pasos) = mcd_pasos (x mod y, y) in
    (d, pasos + 1)
  else
    let (d, pasos) = mcd_pasos (x, y mod x) in
    (d, pasos + 1)