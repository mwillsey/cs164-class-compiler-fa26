type syntax =
  | Lisp
  | Mlb

module type T =
  sig
    type program
    val parse : syntax -> string -> program

    val interp : program -> unit
    val interp_io : input:string -> program -> string

    val compile : program -> Directive.directive list
    val runtime_object_file : string
  end
