let curry f = fun x -> fun y -> f (x, y)

let uncurry g = fun (x, y) -> g x y

uncurry (+);;
let sum = (uncurry (+));;
sum 1;;
sum (2,1);;
let g = curry (function p -> 2 * fst p + 3 * snd p);;
g (2,5);;
let h = g 2;;
h 1, h 2, h 3;;

let comp f g = fun x -> f (g x)

let f = let square x = x * x in comp square ((+) 1);;
f 1, f 2, f 3;;

(* i : 'a -> 'a *)
(*solo existe 1 función: la identidad*)
let i x = x


(* j : 'a * 'b -> 'a *)
(*Únicamente se puede devolver el primer elemento de la tupla. Solo existe 1 función total posible*)
let j (x, _) = x


(* k : 'a * 'b -> 'b*)  
(*Únicamente se puede devolver el segundo elemento de la tupla.
   Solo existe 1 función total posible. *)
let k (_, y) = y


(* l : 'a -> 'a list *)
(*La única lista posible (sin inventar valores de tipo 'a) es la vacía.
   Solo existe 1 función total posible: la función que devuelve [] para cualquier entrada. *)
let l _ = []
