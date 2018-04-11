	.file	"median.c"
	.option nopic
	.text
	.align	2
	.globl	median
	.type	median, @function
median:
	addi	sp,sp,-64
	sd	s0,56(sp)
	addi	s0,sp,64
	mv	a5,a0
	sd	a1,-48(s0)
	sd	a2,-56(s0)
	sw	a5,-36(s0)
	ld	a5,-56(s0)
	sw	zero,0(a5)
	lw	a5,-36(s0)
	slli	a5,a5,2
	addi	a5,a5,-4
	ld	a4,-56(s0)
	add	a5,a4,a5
	sw	zero,0(a5)
	li	a5,1
	sw	a5,-20(s0)
	j	.L2
.L10:
	lw	a5,-20(s0)
	slli	a5,a5,2
	addi	a5,a5,-4
	ld	a4,-48(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-48(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-28(s0)
	lw	a5,-20(s0)
	addi	a5,a5,1
	slli	a5,a5,2
	ld	a4,-48(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-32(s0)
	lw	a4,-24(s0)
	lw	a5,-28(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	bge	a4,a5,.L3
	lw	a4,-28(s0)
	lw	a5,-32(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	bge	a4,a5,.L4
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-28(s0)
	sw	a4,0(a5)
	j	.L7
.L4:
	lw	a4,-32(s0)
	lw	a5,-24(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	bge	a4,a5,.L6
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-24(s0)
	sw	a4,0(a5)
	j	.L7
.L6:
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-32(s0)
	sw	a4,0(a5)
	j	.L7
.L3:
	lw	a4,-24(s0)
	lw	a5,-32(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	bge	a4,a5,.L8
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-24(s0)
	sw	a4,0(a5)
	j	.L7
.L8:
	lw	a4,-32(s0)
	lw	a5,-28(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	bge	a4,a5,.L9
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-28(s0)
	sw	a4,0(a5)
	j	.L7
.L9:
	lw	a5,-20(s0)
	slli	a5,a5,2
	ld	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,-32(s0)
	sw	a4,0(a5)
.L7:
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sw	a5,-20(s0)
.L2:
	lw	a5,-36(s0)
	addiw	a5,a5,-1
	sext.w	a4,a5
	lw	a5,-20(s0)
	sext.w	a5,a5
	blt	a5,a4,.L10
	nop
	ld	s0,56(sp)
	addi	sp,sp,64
	jr	ra
	.size	median, .-median
	.ident	"GCC: (GNU) 7.2.0"
