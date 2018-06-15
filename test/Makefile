.PHONY: all clean

SRCDIR=src
BUILDDIR=build

RISCVCC64=riscv64-unknown-elf-gcc -march=rv64im -mabi=lp64 -static -nostdlib -nostartfiles -mcmodel=medany

all: qsort64 add64

init32.o: init.S
	$(RISCVCC32) -c init.S -o init32.o

init64.o: init.S
	$(RISCVCC64) -c init.S -o init64.o

qsort32: init32.o qsort/qsort_main.c mmio.ld
	$(RISCVCC64) -Tmmio.ld init32.o qsort/qsort_main.c -o qsort32

qsort64: init64.o qsort/qsort_main.c mmio.ld
	$(RISCVCC64) -Tmmio.ld init64.o qsort/qsort_main.c -o qsort64

add32: init32.o add/add.c mmio.ld
	$(RISCVCC64) -Tmmio.ld init32.o add/add.c -o add32

add64: init64.o add/add.c mmio.ld
	$(RISCVCC64) -Tmmio.ld init64.o add/add.c -o add64

# mmio32.o: mmio.c
# 	$(RISCVCC32) -c mmio.c -o mmio32.o

# mmio64.o: mmio.c
# 	$(RISCVCC64) -c mmio.c -o mmio64.o

# $(BUILDDIR)/%32.hex: $(ELF2HEX)/elf2hex $(SRCDIR)/%.c init32.o mmio32.o mmio.ld
# 	mkdir -p $(BUILDDIR)
# 	$(RISCVCC32) -c $(SRCDIR)/$*.c -o intermediate32.o
# 	$(RISCVCC32) -o $(BUILDDIR)/$*32 -Tmmio.ld intermediate32.o init32.o mmio32.o
# 	$(ELF2HEX)/elf2hex $(BUILDDIR)/$*32 0 16G $(BUILDDIR)/$*32.hex
# 	rm intermediate32.o

# $(BUILDDIR)/%64.hex: $(ELF2HEX)/elf2hex $(SRCDIR)/%.c init64.o mmio64.o mmio.ld
# 	mkdir -p $(BUILDDIR)
# 	$(RISCVCC64) -c $(SRCDIR)/$*.c -o intermediate64.o
# 	$(RISCVCC64) -o $(BUILDDIR)/$*64 -Tmmio.ld intermediate64.o init64.o mmio64.o
# 	$(ELF2HEX)/elf2hex $(BUILDDIR)/$*64 0 16G $(BUILDDIR)/$*64.hex
# 	rm intermediate64.o


clean:
	rm -f init32.o init64.o qsort32 qsort64 add32 add64
