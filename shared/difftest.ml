open Printf

module type T = sig
  val run : unit -> unit
end

module Make(I : Infra.T) : T = struct
  type diffresult =
  { program: string
  ; expected: (string, string) result option
  ; interpreter: (string, string) result
  ; compiler: (string, Assemble.error) result }

  type partial_success = {interpreter_agrees: bool; compiler_agrees: bool}

  (* Buckle in, this is kind of a goofy one.
  On the instructional machines, we can't rm -rf directories because
  many of them contain network files (.nfs)—they're not empty! So, we
  recursively remove all files in subdirectories of tmp _not_ starting
  with ".nfs". If tmp does not yet exist, we create it. *)
  let wipe_tmp () =
  let tmpdir = "tmp" in
  let rec rmrf_files path name =
    if String.starts_with ~prefix:".nfs" name then ()
    else if Sys.is_directory path then
      Sys.readdir path
      |> Array.iter (fun name -> rmrf_files (Filename.concat path name) name)
    else Sys.remove path
  in
  if Sys.file_exists tmpdir then rmrf_files tmpdir tmpdir
  else Unix.mkdir tmpdir 0o777

  let indent s =
  String.split_on_char '\n' s
  |> List.map (fun s -> "\t" ^ s)
  |> String.concat "\n"

  let display_diffresult {program; expected; interpreter; compiler} : string =
  let display_outputs outputs =
    outputs
    |> List.map (fun (source, output) ->
          let descriptor, output =
            match output with
            | Ok output ->
                ("output", output)
            | Error error ->
                ("error", error)
          in
          sprintf "%s %s:\n\n%s" source descriptor (indent output))
    |> String.concat "\n\n"
  and compiler =
    Result.map_error
      (function
        | Assemble.Expected error -> error | Assemble.Unexpected error -> error)
      compiler
  in
  let expected =
    match expected with Some expected -> [("Expected", expected)] | None -> []
  and actual = [("Interpreter", interpreter); ("Compiler", compiler)] in
  sprintf "Program:\n\n%s\n\n" (indent program)
  ^ display_outputs (expected @ actual)

  let interpreter_output_matches expected actual =
  match (expected, actual) with
  | Ok expected, Ok actual ->
      String.equal expected actual
  | Error _, Error _ ->
      true
  | Ok _, Error _ | Error _, Ok _ ->
      false

  let compiler_output_matches expected actual =
  match (expected, actual) with
  | Ok expected, Ok actual ->
      String.equal expected actual
  | Error _, Error (Assemble.Expected _) ->
      true
  | Error _, Error (Assemble.Unexpected _) ->
      false
  | Ok _, Error _ | Error _, Ok _ ->
      false

  let result_of_diffresult diffresult =
  let ok, partial_success =
    match diffresult with
    | {program= _; expected= Some expected; interpreter; compiler} ->
        let interpreter_agrees = interpreter_output_matches expected interpreter
        and compiler_agrees = compiler_output_matches expected compiler in
        ( interpreter_agrees && compiler_agrees
        , Some {interpreter_agrees; compiler_agrees} )
    | { program= _
      ; expected= None
      ; interpreter= Ok interpreter
      ; compiler= Ok compiler } ->
        (String.equal interpreter compiler, None)
    | {program= _; expected= None; interpreter= _; compiler= _} ->
        (false, None)
  in
  let summary = display_diffresult diffresult in
  if ok then Ok summary else Error (summary, partial_success)

  let diff is_mlb name program input expected =
  let ast =
    try
      Ok (if is_mlb then I.parse Infra.Mlb program else I.parse Infra.Lisp program)
    with e -> Error (Printexc.to_string e)
  in
  let try_bind f arg =
    Result.bind arg (fun arg ->
        try f arg with e -> Error (Printexc.to_string e))
  in
  let try_map f = try_bind (fun arg -> Ok (f arg)) in
  let interpreter =
    wipe_tmp () ;
    try_map (I.interp_io ~input) ast
  and compiler =
    wipe_tmp () ;
    try_map I.compile ast
    |> function
    | Ok instrs ->
        Assemble.eval_input "test_output" I.runtime_object_file name [] instrs input
    | Error err ->
        Error (Assemble.Expected err)
  in
  wipe_tmp () ;
  result_of_diffresult {program; expected; interpreter; compiler}

  let read_file file =
  let ch = open_in file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; String.trim s

  let diff_file path =
  let filename = Filename.basename path in
  let extension = Filename.extension filename in
  let expected =
    let name = Filename.remove_extension path in
    let out_file = name ^ ".out" and err_file = name ^ ".err" in
    match (Sys.file_exists out_file, Sys.file_exists err_file) with
    | false, false ->
        None
    | false, true ->
        let reason = read_file err_file in
        let description =
          "ERROR"
          ^ if String.length reason > 0 then sprintf ": %s" reason else ""
        in
        Some (Error description)
    | true, false ->
        Some (Ok (read_file out_file))
    | true, true ->
        failwith (sprintf "Expected output and error for test: %s" filename)
  in
  let in_file = Filename.remove_extension path ^ ".in" in
  let input = if Sys.file_exists in_file then read_file in_file else "" in
  diff (extension = ".mlb") filename (read_file path) input expected

  let csv_results =
  (try read_file "../examples/examples.csv" with _ -> "")
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length line != 0)
  |> List.map (String.split_on_char ',')
  |> List.map (List.map String.trim)
  |> List.mapi (fun i ->
        let name = sprintf "anonymous-%d" i in
        function
        | [program] ->
            [(name, diff false name program "" None)]
        | [program; (("error" | "ERROR") as error)] ->
            [(name, diff false name program "" (Some (Error error)))]
        | [program; expected] ->
            [(name, diff false name program "" (Some (Ok expected)))]
        | [program; input; (("error" | "ERROR") as error)] ->
            [(name, diff false name program input (Some (Error error)))]
        | [program; input; expected] ->
            [(name, diff false name program input (Some (Ok expected)))]
        | program :: pairs ->
            let rec diff_multiple i = function
              | [] ->
                  []
              | input :: expected :: rest ->
                  let name = sprintf "%s-%d" name i
                  and expected =
                    match expected with
                    | ("error" | "ERROR") as error ->
                        Error error
                    | output ->
                        Ok output
                  in
                  let result = diff false name program input (Some expected) in
                  (name, result) :: diff_multiple (i + 1) rest
              | _ ->
                  failwith "invalid 'examples.csv' format"
            in
            diff_multiple 0 pairs
        | _ ->
            failwith "invalid 'examples.csv' format")
  |> List.concat

  let mlb_tsv_results =
  (try read_file "../examples/mlb-examples.tsv" with _ -> "")
  |> String.split_on_char '\n'
  |> List.filter (fun line -> String.length line != 0)
  |> List.map (String.split_on_char '\t')
  |> List.map (List.map String.trim)
  |> List.mapi (fun i ->
        let name = sprintf "anonymous-%d" i in
        function
        | [program] ->
            [(name, diff true name program "" None)]
        | [program; (("error" | "ERROR") as error)] ->
            [(name, diff true name program "" (Some (Error error)))]
        | [program; expected] ->
            [(name, diff true name program "" (Some (Ok expected)))]
        | [program; input; (("error" | "ERROR") as error)] ->
            [(name, diff true name program input (Some (Error error)))]
        | [program; input; expected] ->
            [(name, diff true name program input (Some (Ok expected)))]
        | program :: pairs ->
            let rec diff_multiple i = function
              | [] ->
                  []
              | input :: expected :: rest ->
                  let name = sprintf "%s-%d" name i
                  and expected =
                    match expected with
                    | ("error" | "ERROR") as error ->
                        Error error
                    | output ->
                        Ok output
                  in
                  let result = diff true name program input (Some expected) in
                  (name, result) :: diff_multiple (i + 1) rest
              | _ ->
                  failwith "invalid 'mlb-examples.tsv' format"
            in
            diff_multiple 0 pairs
        | _ ->
            failwith "invalid 'mlb-examples.tsv' format")
  |> List.concat

  let file_results =
  Sys.readdir "../examples" |> Array.to_list
  |> List.filter (fun file ->
      Filename.check_suffix file ".lisp" || Filename.check_suffix file ".mlb")
  |> List.map (sprintf "examples/%s")
  |> List.map (fun f -> (f, diff_file (sprintf "../%s" f)))

  let results = file_results @ csv_results @ mlb_tsv_results

  let interp_passes res =
  begin match res with
    | Ok _ ->
        true

    | Error (_, Some partial) ->
        partial.interpreter_agrees

    | Error (_, None) ->
        false
  end

  let compile_passes res =
  begin match res with
    | Ok _ ->
        true

    | Error (_, Some partial) ->
        partial.compiler_agrees

    | Error (_, None) ->
        false
  end

  let difftest () =
  printf "TESTING\n" ;
  results
  |> List.iter (function
      | name, Error (summary, _) ->
          printf "\n=== Test failed: %s ===\n\n%s\n" name summary
      | _, Ok _ ->
          ()) ;
  let num_tests = List.length results in
  let count f l =
    List.fold_left (fun count x -> if f x then 1 + count else count) 0 l
  in
  let failed_tests = count (fun (_, res) -> Result.is_error res) results in
  let interp_passed = count (fun (_, res) -> interp_passes res) results in
  let compile_passed = count (fun (_, res) -> compile_passes res) results in
  if failed_tests = 0 then
    printf "PASSED %d tests\n" num_tests
  else
    printf "\nFAILED %d/%d tests\n(Interpreter passed %d/%d tests; Compiler passed %d/%d tests)"
    failed_tests num_tests interp_passed num_tests compile_passed num_tests

  let difftest_json () =
  List.map
    (fun (name, result) ->
      let result, summary, misc =
        match result with
        | Ok summary ->
            ("passed", summary, [])
        | Error (summary, partial_success) ->
            let partial_success =
              match partial_success with
              | Some {interpreter_agrees; compiler_agrees} ->
                  [ ("interpreter_agrees", `Bool interpreter_agrees)
                  ; ("compiler_agrees", `Bool compiler_agrees) ]
              | None ->
                  []
            in
            ("failed", summary, partial_success)
      in
      [ ("example", `String name)
      ; ("result", `String result)
      ; ("summary", `String summary) ]
      @ misc)
    results
  |> List.map (fun results -> `Assoc results)
  |> fun elts -> `List elts

  let run () =
    match Sys.getenv_opt "DIFFTEST_OUTPUT" with
    | Some "json" ->
        difftest_json () |> Yojson.to_string |> printf "%s"
    | _ ->
        difftest ()
end
