.PHONY: all clean

SRCDIR=src
BUILDDIR=build

RISCVCC64=riscv64-unknown-elf-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany

all: t1

t1 : init.S t1.c build
	$(RISCVCC64) -o $(BUILDDIR)/t1 init.S t1.c

build:
	mkdir -p $(BUILDDIR)

clean:
	rm -rf build
