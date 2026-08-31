let compile (program: string) : string =
  String.concat "\n" [
    "global _entry";
    "_entry:";
    Printf.sprintf "    mov rax, %s" program;
    "    ret"
  ]

let compile_to_file (program: string): unit = 
  let file = open_out "program.s" in
  output_string file (compile program);
  close_out file

let compile_and_run (program: string): string =
  compile_to_file program;
  let _ = Unix.system "nasm program.s -f macho64" in
  let _ = Unix.system "clang -arch x86_64 program.o runtime.c" in
  let input = Unix.open_process_in "./a.out" in
  let response = input_line input in
  close_in input; response