let curry f = fun x -> fun y -> f (x, y)

let uncurry g = fun (x, y) -> g x y



(*uncurry (+);;*)
(*devuelve una funcion (int * int) -> int*)
let sum = (uncurry (+));;
(*sum 1;;*)
(*devuelce un error ya que el valor que se le pasa no es el que espera*)
(*sum (2,1);;*)
(*devuelve la suma de 2 + 1 = 3*)
let g = curry (function p -> 2 * fst p + 3 * snd p);;
(*g (2,5);;*)
(*devuelve un error ya que el valor que se le pasa no es el que espera*)
let h = g 2;;
(*h 1, h 2, h 3;;*)
(*se que va a funcionar, pero no el resultado*)
(*correccion: devuelve un int * int * int = (7, 10, 13)*)



let comp f g = fun x -> f (g x)

let f = let square x = x * x in comp square ((+) 1);;
(*f 1, f 2, f 3;;*)
(*se que va a calcular los cuadrados de un numero, pero no se como sera la salida*)
(* devuelve un int * int * int = (4, 9, 16) *)

(* i : 'a -> 'a *)
(*solo existe 1 función, la identidad*)
let i x = x

(* j : 'a * 'b -> 'a *)
(*solamente existe una funcion posible para este caso, que es, devolver el primer elemento tal cual*)
let j (x, _) = x

(* k : 'a * 'b -> 'b*)  
(*solamente existe una funcion posible para este caso, que es, devolver el segundo elemento tal cual*)
let k (_, y) = y


(* l : 'a -> 'a list *)
(*puedes tener infinitas funciones ya que puedes una funcion que devuelva una lista con el elemento repetido hasta infinitas veces (suponiendo que se tiene memoria infinita)*)
let l x = [x]
let l x = [x; x]
let l x = [x; x; x]
