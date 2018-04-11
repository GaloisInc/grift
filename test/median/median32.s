	.file	"median.c"
	.option nopic
	.text
	.align	2
	.globl	median
	.type	median, @function
median:
	add	sp,sp,-48
	sw	s0,44(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	lw	a5,-44(s0)
	sw	zero,0(a5)
	lw	a4,-36(s0)
	li	a5,1073741824
	add	a5,a5,-1
	add	a5,a4,a5
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	sw	zero,0(a5)
	li	a5,1
	sw	a5,-20(s0)
	j	.L2
.L10:
	lw	a4,-20(s0)
	li	a5,1073741824
	add	a5,a5,-1
	add	a5,a4,a5
	sll	a5,a5,2
	lw	a4,-40(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-40(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-28(s0)
	lw	a5,-20(s0)
	add	a5,a5,1
	sll	a5,a5,2
	lw	a4,-40(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-32(s0)
	lw	a4,-24(s0)
	lw	a5,-28(s0)
	bge	a4,a5,.L3
	lw	a4,-28(s0)
	lw	a5,-32(s0)
	bge	a4,a5,.L4
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-28(s0)
	sw	a4,0(a5)
	j	.L7
.L4:
	lw	a4,-32(s0)
	lw	a5,-24(s0)
	bge	a4,a5,.L6
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-24(s0)
	sw	a4,0(a5)
	j	.L7
.L6:
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-32(s0)
	sw	a4,0(a5)
	j	.L7
.L3:
	lw	a4,-24(s0)
	lw	a5,-32(s0)
	bge	a4,a5,.L8
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-24(s0)
	sw	a4,0(a5)
	j	.L7
.L8:
	lw	a4,-32(s0)
	lw	a5,-28(s0)
	bge	a4,a5,.L9
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-28(s0)
	sw	a4,0(a5)
	j	.L7
.L9:
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-44(s0)
	add	a5,a4,a5
	lw	a4,-32(s0)
	sw	a4,0(a5)
.L7:
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L2:
	lw	a5,-36(s0)
	add	a5,a5,-1
	lw	a4,-20(s0)
	blt	a4,a5,.L10
	nop
	lw	s0,44(sp)
	add	sp,sp,48
	jr	ra
	.size	median, .-median
	.ident	"GCC: (GNU) 7.1.1 20170509"
