	.section ".text"
	.globl main

main:	li x4, 0xabcdabcd
	li x5, 0x20000

	sd x4, 0(x5)
	ld x6, 0(x5)
	ret
