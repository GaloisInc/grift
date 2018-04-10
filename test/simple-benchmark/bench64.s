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
	addi	sp,sp,-32
	sd	s0,24(sp)
	addi	s0,sp,32
	sw	zero,-24(s0)
	sw	zero,-20(s0)
	j	.L2
.L3:
	lw	a5,-24(s0)
	addiw	a5,a5,1
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sw	a5,-20(s0)
.L2:
	li	a5,9998336
	addi	a4,a5,1664
	lw	a5,-20(s0)
	sext.w	a5,a5
	blt	a5,a4,.L3
	lw	a5,-24(s0)
	mv	a0,a5
	ld	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.0"
