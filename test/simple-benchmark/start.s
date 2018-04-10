	.file	"start.s"
	.option nopic
	.text
	.align	2
	.globl	_start
	.type	start, @function
_start:
	addi	sp,zero,0x0
	lui	sp,0x1
	call main
	ecall
