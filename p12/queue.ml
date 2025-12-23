
class ['a] queue = object (self)
  val mutable front = []
  val mutable back = []
  method push (e : 'a) =
    back <- e::back
  method peek =
    match front, back with
      h::t, _ -> Some h
    | [], [] -> None
    | [], _ ->
        front <- List.rev back;
        back <- [];
        self#peek
  method pop =
    match front, back with
      h::t, _ -> front <- t; Some h
    | [], [] -> None
    | [], _ ->
        front <- List.rev back;
        back <- [];
        self#pop
end

