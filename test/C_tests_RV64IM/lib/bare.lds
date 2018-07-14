SECTIONS
{
    __tmp = .;
    .startup 0 : { KEEP(startup.o(.text)) }
    .tbootdata : { KEEP(*(.tbootdata)) }
    . = __tmp;
}
INSERT BEFORE .init;
INPUT(syscalls.o)
INPUT(riscv_counters.o)
INPUT(ns16550.o)
