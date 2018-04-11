	.file	"multiply.c"
	.option nopic
	.text
	.align	2
	.globl	multiply
	.type	multiply, @function
multiply:
	add	sp,sp,-48
	sw	s0,44(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	zero,-24(s0)
	sw	zero,-20(s0)
	j	.L2
.L4:
	lw	a5,-36(s0)
	and	a5,a5,1
	beqz	a5,.L3
	lw	a4,-24(s0)
	lw	a5,-40(s0)
	add	a5,a4,a5
	sw	a5,-24(s0)
.L3:
	lw	a5,-36(s0)
	sra	a5,a5,1
	sw	a5,-36(s0)
	lw	a5,-40(s0)
	sll	a5,a5,1
	sw	a5,-40(s0)
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L2:
	lw	a4,-20(s0)
	li	a5,31
	ble	a4,a5,.L4
	lw	a5,-24(s0)
	mv	a0,a5
	lw	s0,44(sp)
	add	sp,sp,48
	jr	ra
	.size	multiply, .-multiply
	.ident	"GCC: (GNU) 7.1.1 20170509"
