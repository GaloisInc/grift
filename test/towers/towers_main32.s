	.file	"towers_main.c"
	.option nopic
	.comm	g_nodeFreeList,8,4
	.comm	g_nodePool,56,4
	.text
	.align	2
	.globl	list_getSize
	.type	list_getSize, @function
list_getSize:
	add	sp,sp,-32
	sw	s0,28(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	lw	a5,0(a5)
	mv	a0,a5
	lw	s0,28(sp)
	add	sp,sp,32
	jr	ra
	.size	list_getSize, .-list_getSize
	.align	2
	.globl	list_init
	.type	list_init, @function
list_init:
	add	sp,sp,-32
	sw	s0,28(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	sw	zero,0(a5)
	lw	a5,-20(s0)
	sw	zero,4(a5)
	nop
	lw	s0,28(sp)
	add	sp,sp,32
	jr	ra
	.size	list_init, .-list_init
	.align	2
	.globl	list_push
	.type	list_push, @function
list_push:
	add	sp,sp,-48
	sw	s0,44(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lw	a5,4(a5)
	sw	a5,-20(s0)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lw	a5,4(a5)
	lw	a4,4(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	sw	a4,4(a5)
	lw	a5,-36(s0)
	lw	a4,4(a5)
	lw	a5,-20(s0)
	sw	a4,4(a5)
	lw	a5,-36(s0)
	lw	a4,-20(s0)
	sw	a4,4(a5)
	lw	a5,-36(s0)
	lw	a5,4(a5)
	lw	a4,-40(s0)
	sw	a4,0(a5)
	lw	a5,-36(s0)
	lw	a5,0(a5)
	add	a4,a5,1
	lw	a5,-36(s0)
	sw	a4,0(a5)
	nop
	lw	s0,44(sp)
	add	sp,sp,48
	jr	ra
	.size	list_push, .-list_push
	.align	2
	.globl	list_pop
	.type	list_pop, @function
list_pop:
	add	sp,sp,-48
	sw	s0,44(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	lw	a5,-36(s0)
	lw	a5,4(a5)
	lw	a5,0(a5)
	sw	a5,-20(s0)
	lw	a5,-36(s0)
	lw	a5,4(a5)
	sw	a5,-24(s0)
	lw	a5,-36(s0)
	lw	a5,4(a5)
	lw	a4,4(a5)
	lw	a5,-36(s0)
	sw	a4,4(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lw	a4,4(a5)
	lw	a5,-24(s0)
	sw	a4,4(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lw	a4,-24(s0)
	sw	a4,4(a5)
	lw	a5,-36(s0)
	lw	a5,0(a5)
	add	a4,a5,-1
	lw	a5,-36(s0)
	sw	a4,0(a5)
	lw	a5,-20(s0)
	mv	a0,a5
	lw	s0,44(sp)
	add	sp,sp,48
	jr	ra
	.size	list_pop, .-list_pop
	.align	2
	.globl	list_clear
	.type	list_clear, @function
list_clear:
	add	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	j	.L8
.L9:
	lw	a0,-20(s0)
	call	list_pop
.L8:
	lw	a0,-20(s0)
	call	list_getSize
	mv	a5,a0
	bgtz	a5,.L9
	nop
	lw	ra,28(sp)
	lw	s0,24(sp)
	add	sp,sp,32
	jr	ra
	.size	list_clear, .-list_clear
	.align	2
	.globl	towers_init
	.type	towers_init, @function
towers_init:
	add	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	lw	a5,-36(s0)
	lw	a4,-40(s0)
	sw	a4,0(a5)
	lw	a5,-36(s0)
	sw	zero,4(a5)
	lw	a5,-36(s0)
	add	a5,a5,8
	mv	a0,a5
	call	list_init
	lw	a5,-36(s0)
	add	a5,a5,16
	mv	a0,a5
	call	list_init
	lw	a5,-36(s0)
	add	a5,a5,24
	mv	a0,a5
	call	list_init
	sw	zero,-20(s0)
	j	.L11
.L12:
	lw	a5,-36(s0)
	add	a3,a5,8
	lw	a4,-40(s0)
	lw	a5,-20(s0)
	sub	a5,a4,a5
	mv	a1,a5
	mv	a0,a3
	call	list_push
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L11:
	lw	a4,-20(s0)
	lw	a5,-40(s0)
	blt	a4,a5,.L12
	nop
	lw	ra,44(sp)
	lw	s0,40(sp)
	add	sp,sp,48
	jr	ra
	.size	towers_init, .-towers_init
	.align	2
	.globl	towers_clear
	.type	towers_clear, @function
towers_clear:
	add	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	add	a5,a5,8
	mv	a0,a5
	call	list_clear
	lw	a5,-20(s0)
	add	a5,a5,16
	mv	a0,a5
	call	list_clear
	lw	a5,-20(s0)
	add	a5,a5,24
	mv	a0,a5
	call	list_clear
	lw	a5,-20(s0)
	lw	a5,0(a5)
	mv	a1,a5
	lw	a0,-20(s0)
	call	towers_init
	nop
	lw	ra,28(sp)
	lw	s0,24(sp)
	add	sp,sp,32
	jr	ra
	.size	towers_clear, .-towers_clear
	.align	2
	.globl	towers_solve_h
	.type	towers_solve_h, @function
towers_solve_h:
	add	sp,sp,-64
	sw	ra,60(sp)
	sw	s0,56(sp)
	add	s0,sp,64
	sw	a0,-36(s0)
	sw	a1,-40(s0)
	sw	a2,-44(s0)
	sw	a3,-48(s0)
	sw	a4,-52(s0)
	lw	a4,-40(s0)
	li	a5,1
	bne	a4,a5,.L15
	lw	a0,-44(s0)
	call	list_pop
	sw	a0,-20(s0)
	lw	a1,-20(s0)
	lw	a0,-52(s0)
	call	list_push
	lw	a5,-36(s0)
	lw	a5,4(a5)
	add	a4,a5,1
	lw	a5,-36(s0)
	sw	a4,4(a5)
	j	.L17
.L15:
	lw	a5,-40(s0)
	add	a5,a5,-1
	lw	a4,-48(s0)
	lw	a3,-52(s0)
	lw	a2,-44(s0)
	mv	a1,a5
	lw	a0,-36(s0)
	call	towers_solve_h
	lw	a4,-52(s0)
	lw	a3,-48(s0)
	lw	a2,-44(s0)
	li	a1,1
	lw	a0,-36(s0)
	call	towers_solve_h
	lw	a5,-40(s0)
	add	a5,a5,-1
	lw	a4,-52(s0)
	lw	a3,-44(s0)
	lw	a2,-48(s0)
	mv	a1,a5
	lw	a0,-36(s0)
	call	towers_solve_h
.L17:
	nop
	lw	ra,60(sp)
	lw	s0,56(sp)
	add	sp,sp,64
	jr	ra
	.size	towers_solve_h, .-towers_solve_h
	.align	2
	.globl	towers_solve
	.type	towers_solve, @function
towers_solve:
	add	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	add	s0,sp,32
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	lw	a1,0(a5)
	lw	a5,-20(s0)
	add	a2,a5,8
	lw	a5,-20(s0)
	add	a3,a5,16
	lw	a5,-20(s0)
	add	a5,a5,24
	mv	a4,a5
	lw	a0,-20(s0)
	call	towers_solve_h
	nop
	lw	ra,28(sp)
	lw	s0,24(sp)
	add	sp,sp,32
	jr	ra
	.size	towers_solve, .-towers_solve
	.align	2
	.globl	towers_verify
	.type	towers_verify, @function
towers_verify:
	add	sp,sp,-48
	sw	ra,44(sp)
	sw	s0,40(sp)
	add	s0,sp,48
	sw	a0,-36(s0)
	sw	zero,-24(s0)
	lw	a5,-36(s0)
	add	a5,a5,8
	mv	a0,a5
	call	list_getSize
	mv	a5,a0
	beqz	a5,.L20
	li	a5,2
	j	.L21
.L20:
	lw	a5,-36(s0)
	add	a5,a5,16
	mv	a0,a5
	call	list_getSize
	mv	a5,a0
	beqz	a5,.L22
	li	a5,3
	j	.L21
.L22:
	lw	a5,-36(s0)
	add	a5,a5,24
	mv	a0,a5
	call	list_getSize
	mv	a4,a0
	lw	a5,-36(s0)
	lw	a5,0(a5)
	beq	a4,a5,.L23
	li	a5,4
	j	.L21
.L23:
	lw	a5,-36(s0)
	lw	a5,28(a5)
	sw	a5,-20(s0)
	j	.L24
.L26:
	lw	a5,-24(s0)
	add	a5,a5,1
	sw	a5,-24(s0)
	lw	a5,-20(s0)
	lw	a5,0(a5)
	lw	a4,-24(s0)
	beq	a4,a5,.L25
	li	a5,5
	j	.L21
.L25:
	lw	a5,-20(s0)
	lw	a5,4(a5)
	sw	a5,-20(s0)
.L24:
	lw	a5,-20(s0)
	bnez	a5,.L26
	lw	a5,-36(s0)
	lw	a4,4(a5)
	lw	a5,-36(s0)
	lw	a5,0(a5)
	li	a3,1
	sll	a5,a3,a5
	add	a5,a5,-1
	beq	a4,a5,.L27
	li	a5,6
	j	.L21
.L27:
	li	a5,0
.L21:
	mv	a0,a5
	lw	ra,44(sp)
	lw	s0,40(sp)
	add	sp,sp,48
	jr	ra
	.size	towers_verify, .-towers_verify
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
	lui	a5,%hi(g_nodeFreeList)
	addi	a0,a5,%lo(g_nodeFreeList)
	call	list_init
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lui	a4,%hi(g_nodePool)
	addi	a4,a4,%lo(g_nodePool)
	sw	a4,4(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	li	a4,7
	sw	a4,0(a5)
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	sw	zero,52(a5)
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	li	a4,99
	sw	a4,48(a5)
	sw	zero,-20(s0)
	j	.L29
.L30:
	lw	a5,-20(s0)
	add	a5,a5,1
	sll	a4,a5,3
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	add	a4,a4,a5
	lui	a3,%hi(g_nodePool)
	lw	a5,-20(s0)
	addi	a3,a3,%lo(g_nodePool)
	sll	a5,a5,3
	add	a5,a3,a5
	sw	a4,4(a5)
	lui	a5,%hi(g_nodePool)
	lw	a4,-20(s0)
	sll	a4,a4,3
	addi	a5,a5,%lo(g_nodePool)
	add	a5,a4,a5
	lw	a4,-20(s0)
	sw	a4,0(a5)
	lw	a5,-20(s0)
	add	a5,a5,1
	sw	a5,-20(s0)
.L29:
	lw	a4,-20(s0)
	li	a5,5
	ble	a4,a5,.L30
	add	a5,s0,-52
	li	a1,7
	mv	a0,a5
	call	towers_init
	add	a5,s0,-52
	mv	a0,a5
	call	towers_clear
	add	a5,s0,-52
	mv	a0,a5
	call	towers_solve
	add	a5,s0,-52
	mv	a0,a5
	call	towers_verify
	mv	a5,a0
	mv	a0,a5
	lw	ra,76(sp)
	lw	s0,72(sp)
	add	sp,sp,80
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
