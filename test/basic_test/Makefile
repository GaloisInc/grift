.PHONY: all clean

SRCDIR=src
BUILDDIR=build

RISCVCC64=riscv64-unknown-elf-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany

all: basic_test64

basic_test64 : init.S basic_test.s build
	$(RISCVCC64) -o $(BUILDDIR)/basic_test64 init.S basic_test.s

build:
	mkdir -p $(BUILDDIR)

clean:
	rm -rf build
