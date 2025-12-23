let pi = fun _ -> atan(1.) *. 4.

let e = fun _ -> exp(1.0)

let max_int_f = fun _ -> float_of_int max_int

let perimeter = fun r -> 2. *. pi() *. r

let area = fun r -> pi() *. r ** 2.

let next_char = fun c -> Char.chr (Char.code c + 1)

let abs_f = fun x -> if x < 0. then -.x else x

let odd = fun x -> x mod 2 <> 0

let next_5_mult = fun x -> ((x / 5) + 1) * 5

let is_letter = fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let string_of_bool = fun b -> if b then "verdadero" else "falso"

