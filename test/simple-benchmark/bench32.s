	.file	"bench.c"
	.option nopic
	.globl	ITERS
	.section	.sdata,"aw",@progbits
	.align	2
	.type	ITERS, @object
	.size	ITERS, 4
ITERS:
	.word	10000000
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	add	sp,sp,-32
	sw	s0,28(sp)
	add	s0,sp,32
	sw	zero,-24(s0)
	sw	zero,-20(s0)
	j	.L2
.L3:
	lw	a5,-24(s0)
	add	a5,a5,1
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L2:
	li	a5,9998336
	add	a5,a5,1664
	lw	a4,-20(s0)
	blt	a4,a5,.L3
	lw	a5,-24(s0)
	mv	a0,a5
	lw	s0,28(sp)
	add	sp,sp,32
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
