# Class Compiler

This is the compiler that we built in lecture.
I will keep it up to date as we move through the course.

Changes won't necessarily be purely additive, so 
you may want to look at the commit history 
if you want a point-in-time snapshot.

## Our first compiler!

How to see the assembly for the C program:
```bash
clang -arch x86_64 -S runtime.c
```

Bit by bit:
- `clang` is your compiler, you may use `gcc` as well (on Mac they might be the same!).
- `-arch x86_64` is the architecture you want to compile for.
  In this class we will target the x86_64 architecture.
  On a x86_64 machine, you can omit this.
- `-S` is the flag that tells the compiler to output the assembly code, rather than compiling 
  "all the way" to an executable or object file.
- `runtime.c` is the C program you want to compile.

And here's our first snippet of assembly:

```asm
global _entry
_entry:
    mov rax, 400
    ret
```

And to "compile" (or assemble) assembly to an object file, we'll use the `nasm` assembler:

```sh
# -f macho64 needed for mac
nasm -f macho64 program.s
```

We can compile it together with the runtime:

```sh
clang -arch x86_64 runtime.c program.o
```

Ignore the `dune` stuff for now, we will get to the bits about how to actually write OCaml later.

To run, do `dune utop`, then `open Cs164.Compile`, then `compile_and_run "50"`. 