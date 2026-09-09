# the compiler emits x86-64, so the runtime has to match it.  on apple silicon
# that means cross-compiling and running the result under rosetta; on the
# instructional machines this is a no-op.
ifeq ($(shell uname -s),Darwin)
CFLAGS += -arch x86_64
endif

default: runtime.o

runtime.o: runtime.c
