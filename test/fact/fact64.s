	.file	"fact.c"
	.option nopic
	.globl	INPUT
	.section	.sdata,"aw",@progbits
	.align	3
	.type	INPUT, @object
	.size	INPUT, 8
INPUT:
	.dword	20
	.text
	.align	2
	.globl	fact
	.type	fact, @function
fact:
	addi	sp,sp,-48
	sd	s0,40(sp)
	addi	s0,sp,48
	sd	a0,-40(s0)
	li	a5,1
	sd	a5,-24(s0)
	j	.L2
.L3:
	ld	a4,-24(s0)
	ld	a5,-40(s0)
	mul	a5,a4,a5
	sd	a5,-24(s0)
	ld	a5,-40(s0)
	addi	a5,a5,-1
	sd	a5,-40(s0)
.L2:
	ld	a5,-40(s0)
	bgtz	a5,.L3
	ld	a5,-24(s0)
	mv	a0,a5
	ld	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	fact, .-fact
	.align	2
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-16
	sd	ra,8(sp)
	sd	s0,0(sp)
	addi	s0,sp,16
	li	a5,20
	mv	a0,a5
	call	fact
	mv	a5,a0
	mv	a0,a5
	ld	ra,8(sp)
	ld	s0,0(sp)
	addi	sp,sp,16
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.0"
