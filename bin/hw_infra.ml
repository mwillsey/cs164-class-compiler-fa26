open Shared
(* open Error *)

module I : Infra.T = struct
  type program = S_exp.s_exp list

  let parse syntax s =
    begin match syntax with
    | Infra.Lisp -> S_exp.parse_many s
    | Infra.Mlb -> failwith "Cannot parse .mlb files for this homework"
    end

  (* let interp prog = Printf.printf "%s%!" (Lib.Interp.interp prog)
  let interp_io ~input:_ prog = Lib.Interp.interp prog *)
  let interp = Lib.Interp.interp
  let interp_io ~input prog = Lib.Interp.interp_io prog input
  let compile = Lib.Compile.compile
  let runtime_object_file = Lib.Runtime.runtime
end
