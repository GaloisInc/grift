# -*- makefile -*-

TOPDIR             ?= .
SUBDIRS             = . \
                      hello \
                      hello2 \
                      hello_w_math \
                      accel_aes \
                      accel_aes-fixed


# CROSS_COMPILE      ?= riscv32-unknown-elf-
CROSS_COMPILE      ?= riscv64-unknown-elf-

# Cross compile programs
AR                  = $(CROSS_COMPILE)ar
CC                  = $(CROSS_COMPILE)gcc
LD                  = $(CROSS_COMPILE)ld
OBJCOPY             = $(CROSS_COMPILE)objcopy
OBJDUMP             = $(CROSS_COMPILE)objdump
STRIP               = $(CROSS_COMPILE)strip

ifneq (,$(findstring s,$(MAKEFLAGS)))
ARFLAGS             = crs
else
ARFLAGS             = crsv
endif

# RV32IM
# ARCH = -march=rv32im
# ABI = -mabi=ilp32

# RV64IM
ARCH = -march=rv64im
ABI = -mabi=lp64

ASFLAGS            += -g $(ARCH) $(ABI)

CFLAGS             += -g -I$(TOPDIR)/lib $(ARCH) $(ABI)
CFLAGS             += -specs=lib/bare.specs
CFLAGS             += -DCONSOLE_UART

SRC_EXT             = c cpp cxx cc
EXTRA_EXT           = text
CLEAN_EXTRA_EXT     = map

TARGETS_SKIP        =

TARGETS             = $(TOPDIR)/lib/startup.o $(TOPDIR)/lib/syscalls.o $(TOPDIR)/lib/riscv_counters.o $(TOPDIR)/lib/ns16550.o $(filter-out $(TARGETS_SKIP),$(foreach subdir,$(SUBDIRS),$(foreach srcext,$(SRC_EXT),$(patsubst %.$(srcext),%,$(wildcard $(subdir)/*.$(srcext))))))
TARGETS_EXTRA       = $(foreach ext,$(EXTRA_EXT), $(addsuffix .$(ext), $(TARGETS)))
CLEAN_EXTRA         = $(foreach ext,$(CLEAN_EXTRA_EXT), $(addsuffix .$(ext), $(TARGETS)))


ASFLAGS            += -Wa,-Ilegacy

LDLIBS             += -lm

LDFLAGS            += -L$(TOPDIR)/lib
LDFLAGS	           += -Wl,-e,_reset
LDFLAGS            += -Wl,-T$(TOPDIR)/lib/bare.lds
LDFLAGS            += -Wl,-Map,$@.map



default:all

all: $(TARGETS) $(TARGETS_EXTRA)

clean distclean:
	$(RM) $(TARGETS) $(TARGETS_EXTRA) $(CLEAN_EXTRA)

%.text: %
	$(OBJDUMP) --all-headers --disassemble --source $^ > $@
#	$(OBJDUMP) --full-contents --disassemble --source $^ > $@

.PHONY: default all clean distclean
