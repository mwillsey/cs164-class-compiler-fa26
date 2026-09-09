open Error
open Core

module Make(I : Infra.T) = struct
  let compile_command =
    Command.basic ~summary:"Compile the given file to an executable"
      Command.Let_syntax.(
        let%map_open filename = anon ("filename" %: Command.Param.string)
        and directory = anon ("directory" %: Command.Param.string)
        and run = flag "-r" no_arg ~doc:"run the binary" in
        fun () ->
          try
            let text = In_channel.read_all filename in
            let ast =
              if Filename.check_suffix filename ".mlb" then I.parse Infra.Mlb text
              else I.parse Lisp text
            in
            let instrs = I.compile ast in
            let filename = Filename.basename filename in
            if run then
              Assemble.eval directory I.runtime_object_file filename [] instrs
              |> function
              | Ok output ->
                  printf "%s\n" output
              | Error (Expected error | Unexpected error) ->
                  eprintf "%s\n" error
            else
              Assemble.build directory I.runtime_object_file filename instrs |> ignore
          with Stuck _ as e ->
            Printf.eprintf "Error: %s\n" (Exn.to_string e))

  let compile () = Command_unix.run ~version:"1.0" compile_command

  let interp_command =
    Command.basic ~summary:"Interpret the given file"
      Command.Let_syntax.(
        let%map_open filename = anon (maybe ("filename" %: Command.Param.string))
        and expression =
          flag "-e" (optional string) ~doc:"lisp expression to evaluate"
        in
        fun () ->
          try
            match (filename, expression) with
            | Some f, _ ->
                let text = In_channel.read_all f in
                let ast =
                  if Filename.check_suffix f ".mlb" then I.parse Infra.Mlb text
                  else I.parse Lisp text
                in
                ast |> I.interp ; print_endline ""
            | _, Some e ->
              I.parse Infra.Lisp e |> I.interp ;
                print_endline ""
            | _ ->
                Printf.eprintf
                  "Error: must specify either an expression to evaluate or a file\n"
          with e -> Printf.eprintf "Error: %s\n" (Exn.to_string e))

  let interp () = Command_unix.run ~version:"1.0" interp_command
  end
