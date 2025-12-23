let[@tail_mod_cons] rec front = function
[] -> raise (Failure "front")
| h::[] -> []
| h::t -> h :: front t;;

let[@tail_mod_cons] rec compress = function
h1::h2::t ->
if h1 = h2 then compress (h2::t)
else h1 :: compress (h2::t)
| l -> l;;

(*let[@tail_mod_cons] fold_right =
List.fold_right*)
(* Tail Modulo Constructor no se puede aplicar en fold_righy, ya que no es una funcion recursiva y tampoco puede usarse dentro de otras fuciones que se llaman en la funcion aplicada, es decir Tail Modulo Constructor no puede modificar fold_right internamente*)

  (* TODO: COMPARAR ESTAS DEFINICIONES CON LAS DEL EJERCICIO ANTERIOR 
  MIRAR SU RENDIMIENTO SOBRE LISTAS LARGAS Y ESCRIBIR CONCLUSIONES*)