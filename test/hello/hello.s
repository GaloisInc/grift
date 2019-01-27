	.file	"hello.c"
	.option nopic
	.text
	.section	.rodata
	.align	3
.LC0:
	.string	"Hi\n"
	.text
	.align	1
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-16
	sd	ra,8(sp)
	sd	s0,0(sp)
	addi	s0,sp,16
	li	a2,3
	lui	a5,%hi(.LC0)
	addi	a1,a5,%lo(.LC0)
	li	a0,0
	call	write
	li	a5,0
	mv	a0,a5
	ld	ra,8(sp)
	ld	s0,0(sp)
	addi	sp,sp,16
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 8.2.0"
