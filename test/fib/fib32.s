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
	add	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	sw	s1,20(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	bgtz	a5,.L2
	li	a5,0
	j	.L3
.L2:
	lw	a4,-20(s0)
	li	a5,1
	bne	a4,a5,.L4
	li	a5,1
	j	.L3
.L4:
	lw	a5,-20(s0)
	add	a5,a5,-1
	mv	a0,a5
	call	fib
	mv	s1,a0
	lw	a5,-20(s0)
	add	a5,a5,-2
	mv	a0,a5
	call	fib
	mv	a5,a0
	add	a5,s1,a5
.L3:
	mv	a0,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	lw	s1,20(sp)
	add	sp,sp,32
	jr	ra
	.size	fib, .-fib
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
	call	fib
	mv	a5,a0
	mv	a0,a5
	lw	ra,12(sp)
	lw	s0,8(sp)
	add	sp,sp,16
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
