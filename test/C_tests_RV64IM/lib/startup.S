	.text
	.globl	_reset
	.org	0x200

_reset:
	lui	sp, 0x0100
	ori	sp, sp, 0x0000
#if __riscv_xlen == 64
	sd	zero, 0(sp)
	sd	zero, -8(sp)
#else
	sw	zero, 0(sp)
	sw	zero, -4(sp)
#endif
	li      s0, 0x3006
	csrw    mstatus, s0
	li	s0, 0x0100
#if __riscv_xlen == 64
	sd	zero, 0xC0(s0)
#else
	sw	zero, 0xC0(s0)
#endif
	csrw	mtvec, s0
#ifdef __riscv_float_abi_double
	li	s0, 0x1000
	csrs	mstatus, s0
#endif
	j _start
