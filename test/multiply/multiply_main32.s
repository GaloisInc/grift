	.file	"multiply_main.c"
	.option nopic
	.text
	.align	2
	.type	verify, @function
verify:
	add	sp,sp,-64
	sw	s0,60(sp)
	add	s0,sp,64
	sw	a0,-52(s0)
	sw	a1,-56(s0)
	sw	a2,-60(s0)
	sw	zero,-20(s0)
	j	.L2
.L6:
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-56(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	add	a5,a5,1
	sll	a5,a5,2
	lw	a4,-56(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-28(s0)
	lw	a5,-20(s0)
	sll	a5,a5,2
	lw	a4,-60(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-32(s0)
	lw	a5,-20(s0)
	add	a5,a5,1
	sll	a5,a5,2
	lw	a4,-60(s0)
	add	a5,a4,a5
	lw	a5,0(a5)
	sw	a5,-36(s0)
	lw	a4,-24(s0)
	lw	a5,-32(s0)
	beq	a4,a5,.L3
	lw	a5,-20(s0)
	add	a5,a5,1
	j	.L4
.L3:
	lw	a4,-28(s0)
	lw	a5,-36(s0)
	beq	a4,a5,.L5
	lw	a5,-20(s0)
	add	a5,a5,2
	j	.L4
.L5:
	lw	a5,-20(s0)
	add	a5,a5,2
	sw	a5,-20(s0)
.L2:
	lw	a5,-52(s0)
	srl	a4,a5,31
	add	a5,a4,a5
	sra	a5,a5,1
	sll	a5,a5,1
	lw	a4,-20(s0)
	blt	a4,a5,.L6
	lw	a5,-52(s0)
	and	a5,a5,1
	beqz	a5,.L7
	lw	a4,-52(s0)
	li	a5,1073741824
	add	a5,a5,-1
	add	a5,a4,a5
	sll	a5,a5,2
	lw	a4,-56(s0)
	add	a5,a4,a5
	lw	a4,0(a5)
	lw	a3,-52(s0)
	li	a5,1073741824
	add	a5,a5,-1
	add	a5,a3,a5
	sll	a5,a5,2
	lw	a3,-60(s0)
	add	a5,a3,a5
	lw	a5,0(a5)
	beq	a4,a5,.L7
	lw	a5,-52(s0)
	j	.L4
.L7:
	li	a5,0
.L4:
	mv	a0,a5
	lw	s0,60(sp)
	add	sp,sp,64
	jr	ra
	.size	verify, .-verify
	.globl	input_data1
	.data
	.align	2
	.type	input_data1, @object
	.size	input_data1, 40
input_data1:
	.word	41
	.word	454
	.word	833
	.word	335
	.word	564
	.word	1
	.word	187
	.word	989
	.word	749
	.word	365
	.globl	input_data2
	.align	2
	.type	input_data2, @object
	.size	input_data2, 40
input_data2:
	.word	350
	.word	572
	.word	132
	.word	64
	.word	949
	.word	153
	.word	584
	.word	216
	.word	805
	.word	140
	.globl	verify_data
	.align	2
	.type	verify_data, @object
	.size	verify_data, 40
verify_data:
	.word	14350
	.word	259688
	.word	109956
	.word	21440
	.word	535236
	.word	154
	.word	109208
	.word	213624
	.word	602945
	.word	51100
	.text
	.align	2
	.globl	main
	.type	main, @function
main:
	add	sp,sp,-80
	sw	ra,76(sp)
	sw	s0,72(sp)
	add	s0,sp,80
	sw	a0,-68(s0)
	sw	a1,-72(s0)
	sw	zero,-20(s0)
	j	.L9
.L10:
	lui	a5,%hi(input_data1)
	lw	a4,-20(s0)
	sll	a4,a4,2
	addi	a5,a5,%lo(input_data1)
	add	a5,a4,a5
	lw	a3,0(a5)
	lui	a5,%hi(input_data2)
	lw	a4,-20(s0)
	sll	a4,a4,2
	addi	a5,a5,%lo(input_data2)
	add	a5,a4,a5
	lw	a5,0(a5)
	mv	a1,a5
	mv	a0,a3
	call	multiply
	mv	a4,a0
	lw	a5,-20(s0)
	sll	a5,a5,2
	add	a3,s0,-16
	add	a5,a3,a5
	sw	a4,-44(a5)
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L9:
	lw	a4,-20(s0)
	li	a5,9
	ble	a4,a5,.L10
	add	a4,s0,-60
	lui	a5,%hi(verify_data)
	addi	a2,a5,%lo(verify_data)
	mv	a1,a4
	li	a0,10
	call	verify
	mv	a5,a0
	mv	a0,a5
	lw	ra,76(sp)
	lw	s0,72(sp)
	add	sp,sp,80
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
