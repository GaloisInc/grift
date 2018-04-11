	.file	"multiply.c"
	.option nopic
	.text
	.align	2
	.globl	multiply
	.type	multiply, @function
multiply:
	addi	sp,sp,-48
	sd	s0,40(sp)
	addi	s0,sp,48
	mv	a5,a0
	mv	a4,a1
	sw	a5,-36(s0)
	mv	a5,a4
	sw	a5,-40(s0)
	sw	zero,-24(s0)
	sw	zero,-20(s0)
	j	.L2
.L4:
	lw	a5,-36(s0)
	andi	a5,a5,1
	sext.w	a5,a5
	beqz	a5,.L3
	lw	a4,-24(s0)
	lw	a5,-40(s0)
	addw	a5,a4,a5
	sw	a5,-24(s0)
.L3:
	lw	a5,-36(s0)
	sraiw	a5,a5,1
	sw	a5,-36(s0)
	lw	a5,-40(s0)
	slliw	a5,a5,1
	sw	a5,-40(s0)
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sw	a5,-20(s0)
.L2:
	lw	a5,-20(s0)
	sext.w	a4,a5
	li	a5,31
	ble	a4,a5,.L4
	lw	a5,-24(s0)
	mv	a0,a5
	ld	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	multiply, .-multiply
	.ident	"GCC: (GNU) 7.2.0"
