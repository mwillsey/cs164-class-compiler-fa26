open Shared
(* open Error *)

module I : Infra.T = struct
  type program = S_exp.s_exp
  let parse syntax s =
    begin match syntax with
      | Infra.Lisp -> S_exp.parse s
      | Infra.Mlb -> failwith "Cannot parse .mlb files for this homework"
    end

  let interp prog = Printf.printf "%s%!" (Lib.Interp.interp prog)
  let interp_io ~input:_ prog = Lib.Interp.interp prog

  let compile = Lib.Compile.compile
  let runtime_object_file = Lib.Runtime.runtime
end
