	.file	"fib.c"
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
	.globl	fib
	.type	fib, @function
fib:
	addi	sp,sp,-48
	sd	ra,40(sp)
	sd	s0,32(sp)
	sd	s1,24(sp)
	addi	s0,sp,48
	mv	a5,a0
	sw	a5,-36(s0)
	lw	a5,-36(s0)
	sext.w	a5,a5
	bgtz	a5,.L2
	li	a5,0
	j	.L3
.L2:
	lw	a5,-36(s0)
	sext.w	a4,a5
	li	a5,1
	bne	a4,a5,.L4
	li	a5,1
	j	.L3
.L4:
	lw	a5,-36(s0)
	addiw	a5,a5,-1
	sext.w	a5,a5
	mv	a0,a5
	call	fib
	mv	a5,a0
	mv	s1,a5
	lw	a5,-36(s0)
	addiw	a5,a5,-2
	sext.w	a5,a5
	mv	a0,a5
	call	fib
	mv	a5,a0
	addw	a5,s1,a5
	sext.w	a5,a5
.L3:
	mv	a0,a5
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	addi	sp,sp,48
	jr	ra
	.size	fib, .-fib
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
	call	fib
	mv	a5,a0
	mv	a0,a5
	ld	ra,8(sp)
	ld	s0,0(sp)
	addi	sp,sp,16
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.0"
