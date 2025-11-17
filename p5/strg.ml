let rec str3_of_int n =
  if n < 3 then
    String.make 1 (char_of_int (n + int_of_char '0'))
  else
    let q = n / 3 in
    let r = n mod 3 in
    (str3_of_int q) ^ (str3_of_int r)

let int_of_str3 s =
  let base = 3 in
  let modn = 1 lsl Sys.int_size in
  let rec aux i acc =
    if i = String.length s then acc
    else
      let digit = (int_of_char s.[i]) - (int_of_char '0') in
      let acc' = (acc * base + digit) mod modn in
      aux (i + 1) acc'
  in
  aux 0 0

let rec strg_of_int b n =
  if n < b then
    String.make 1 (char_of_int (n + int_of_char '0'))
  else
    let q = n / b in
    let r = n mod b in
    (strg_of_int b q) ^ (strg_of_int b r)

let int_of_strg b s =
  let modn = 1 lsl Sys.int_size in
  let rec aux i acc =
    if i = String.length s then acc
    else
      let digit = (int_of_char s.[i]) - (int_of_char '0') in
      let acc' = (acc * b + digit) mod modn in
      aux (i + 1) acc'
  in
  aux 0 0
