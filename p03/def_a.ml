let pi _ = atan(1.) *. 4.

let e _ = exp(1.0)

let max_int_f _ = float_of_int max_int

let perimeter r = fun r -> 2. *. pi() *. r

let area r = pi() *. r ** 2.

let next_char c = Char.chr (Char.code c + 1)

let abs_f x = if x < 0. then -.x else x

let odd x = x mod 2 <> 0

let next_5_mult x = ((x / 5) + 1) * 5

let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let string_of_bool b = if b then "true" else "false"