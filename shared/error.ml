exception Stuck of S_exp.s_exp
exception UndefinedBehavior of string

let () =
  Printexc.register_printer (function
    | Stuck e ->
        Some (Printf.sprintf "Stuck[%s]" (S_exp.show e))
    | _ ->
        None)
