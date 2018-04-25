	.file	"fact.c"
	.option nopic
	.globl	INPUT
	.section	.sdata,"aw",@progbits
	.align	2
	.type	INPUT, @object
	.size	INPUT, 4
INPUT:
	.word	20
	.text
	.align	2
	.globl	fact
	.type	fact, @function
fact:
	add	sp,sp,-48
	sw	s0,44(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	li	a5,1
	sw	a5,-20(s0)
	j	.L2
.L3:
	lw	a4,-20(s0)
	lw	a5,-36(s0)
	mul	a5,a4,a5
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	add	a5,a5,-1
	sw	a5,-36(s0)
.L2:
	lw	a5,-36(s0)
	bgtz	a5,.L3
	lw	a5,-20(s0)
	mv	a0,a5
	lw	s0,44(sp)
	add	sp,sp,48
	jr	ra
	.size	fact, .-fact
	.align	2
	.globl	main
	.type	main, @function
main:
	add	sp,sp,-16
	sw	ra,12(sp)
	sw	s0,8(sp)
	add	s0,sp,16
	li	a5,20
	mv	a0,a5
	call	fact
	mv	a5,a0
	mv	a0,a5
	lw	ra,12(sp)
	lw	s0,8(sp)
	add	sp,sp,16
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
