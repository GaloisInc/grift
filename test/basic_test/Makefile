.PHONY: all clean

SRCDIR=src
BUILDDIR=build

RISCVCC32=riscv32-unknown-elf-gcc -march=rv32im -mabi=ilp32 -static -nostdlib -nostartfiles -mcmodel=medany
RISCVCC64=riscv64-unknown-elf-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany

all: basic_test64 basic_test32

basic_test32 : init.S basic_test.s build
	$(RISCVCC32) -o $(BUILDDIR)/basic_test32 init.S basic_test32.s

basic_test64 : init.S basic_test.s build
	$(RISCVCC64) -o $(BUILDDIR)/basic_test64 init.S basic_test.s

build:
	mkdir -p $(BUILDDIR)

clean:
	rm -rf build
