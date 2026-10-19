exception Stuck of S_exp.s_exp
exception UndefinedBehavior of string

let () =
  Printexc.register_printer (function
    | Stuck e ->
        Some (Printf.sprintf "Stuck[%s]" (S_exp.string_of_s_exp e))
    | _ ->
        None)
