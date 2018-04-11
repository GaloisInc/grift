	.file	"towers_main.c"
	.option nopic
	.comm	g_nodeFreeList,16,8
	.comm	g_nodePool,112,8
	.text
	.align	2
	.globl	list_getSize
	.type	list_getSize, @function
list_getSize:
	addi	sp,sp,-32
	sd	s0,24(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	ld	a5,-24(s0)
	lw	a5,0(a5)
	mv	a0,a5
	ld	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	list_getSize, .-list_getSize
	.align	2
	.globl	list_init
	.type	list_init, @function
list_init:
	addi	sp,sp,-32
	sd	s0,24(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	ld	a5,-24(s0)
	sw	zero,0(a5)
	ld	a5,-24(s0)
	sd	zero,8(a5)
	nop
	ld	s0,24(sp)
	addi	sp,sp,32
	jr	ra
	.size	list_init, .-list_init
	.align	2
	.globl	list_push
	.type	list_push, @function
list_push:
	addi	sp,sp,-48
	sd	s0,40(sp)
	addi	s0,sp,48
	sd	a0,-40(s0)
	mv	a5,a1
	sw	a5,-44(s0)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	ld	a5,8(a5)
	sd	a5,-24(s0)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	ld	a5,8(a5)
	ld	a4,8(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	sd	a4,8(a5)
	ld	a5,-40(s0)
	ld	a4,8(a5)
	ld	a5,-24(s0)
	sd	a4,8(a5)
	ld	a5,-40(s0)
	ld	a4,-24(s0)
	sd	a4,8(a5)
	ld	a5,-40(s0)
	ld	a5,8(a5)
	lw	a4,-44(s0)
	sw	a4,0(a5)
	ld	a5,-40(s0)
	lw	a5,0(a5)
	addiw	a5,a5,1
	sext.w	a4,a5
	ld	a5,-40(s0)
	sw	a4,0(a5)
	nop
	ld	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	list_push, .-list_push
	.align	2
	.globl	list_pop
	.type	list_pop, @function
list_pop:
	addi	sp,sp,-48
	sd	s0,40(sp)
	addi	s0,sp,48
	sd	a0,-40(s0)
	ld	a5,-40(s0)
	ld	a5,8(a5)
	lw	a5,0(a5)
	sw	a5,-20(s0)
	ld	a5,-40(s0)
	ld	a5,8(a5)
	sd	a5,-32(s0)
	ld	a5,-40(s0)
	ld	a5,8(a5)
	ld	a4,8(a5)
	ld	a5,-40(s0)
	sd	a4,8(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	ld	a4,8(a5)
	ld	a5,-32(s0)
	sd	a4,8(a5)
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	ld	a4,-32(s0)
	sd	a4,8(a5)
	ld	a5,-40(s0)
	lw	a5,0(a5)
	addiw	a5,a5,-1
	sext.w	a4,a5
	ld	a5,-40(s0)
	sw	a4,0(a5)
	lw	a5,-20(s0)
	mv	a0,a5
	ld	s0,40(sp)
	addi	sp,sp,48
	jr	ra
	.size	list_pop, .-list_pop
	.align	2
	.globl	list_clear
	.type	list_clear, @function
list_clear:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	j	.L8
.L9:
	ld	a0,-24(s0)
	call	list_pop
.L8:
	ld	a0,-24(s0)
	call	list_getSize
	mv	a5,a0
	bgtz	a5,.L9
	nop
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	jr	ra
	.size	list_clear, .-list_clear
	.align	2
	.globl	towers_init
	.type	towers_init, @function
towers_init:
	addi	sp,sp,-48
	sd	ra,40(sp)
	sd	s0,32(sp)
	addi	s0,sp,48
	sd	a0,-40(s0)
	mv	a5,a1
	sw	a5,-44(s0)
	ld	a5,-40(s0)
	lw	a4,-44(s0)
	sw	a4,0(a5)
	ld	a5,-40(s0)
	sw	zero,4(a5)
	ld	a5,-40(s0)
	addi	a5,a5,8
	mv	a0,a5
	call	list_init
	ld	a5,-40(s0)
	addi	a5,a5,24
	mv	a0,a5
	call	list_init
	ld	a5,-40(s0)
	addi	a5,a5,40
	mv	a0,a5
	call	list_init
	sw	zero,-20(s0)
	j	.L11
.L12:
	ld	a5,-40(s0)
	addi	a3,a5,8
	lw	a4,-44(s0)
	lw	a5,-20(s0)
	subw	a5,a4,a5
	sext.w	a5,a5
	mv	a1,a5
	mv	a0,a3
	call	list_push
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sw	a5,-20(s0)
.L11:
	lw	a4,-20(s0)
	lw	a5,-44(s0)
	sext.w	a4,a4
	sext.w	a5,a5
	blt	a4,a5,.L12
	nop
	ld	ra,40(sp)
	ld	s0,32(sp)
	addi	sp,sp,48
	jr	ra
	.size	towers_init, .-towers_init
	.align	2
	.globl	towers_clear
	.type	towers_clear, @function
towers_clear:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	ld	a5,-24(s0)
	addi	a5,a5,8
	mv	a0,a5
	call	list_clear
	ld	a5,-24(s0)
	addi	a5,a5,24
	mv	a0,a5
	call	list_clear
	ld	a5,-24(s0)
	addi	a5,a5,40
	mv	a0,a5
	call	list_clear
	ld	a5,-24(s0)
	lw	a5,0(a5)
	mv	a1,a5
	ld	a0,-24(s0)
	call	towers_init
	nop
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	jr	ra
	.size	towers_clear, .-towers_clear
	.align	2
	.globl	towers_solve_h
	.type	towers_solve_h, @function
towers_solve_h:
	addi	sp,sp,-80
	sd	ra,72(sp)
	sd	s0,64(sp)
	addi	s0,sp,80
	sd	a0,-40(s0)
	mv	a5,a1
	sd	a2,-56(s0)
	sd	a3,-64(s0)
	sd	a4,-72(s0)
	sw	a5,-44(s0)
	lw	a5,-44(s0)
	sext.w	a4,a5
	li	a5,1
	bne	a4,a5,.L15
	ld	a0,-56(s0)
	call	list_pop
	mv	a5,a0
	sw	a5,-20(s0)
	lw	a5,-20(s0)
	mv	a1,a5
	ld	a0,-72(s0)
	call	list_push
	ld	a5,-40(s0)
	lw	a5,4(a5)
	addiw	a5,a5,1
	sext.w	a4,a5
	ld	a5,-40(s0)
	sw	a4,4(a5)
	j	.L17
.L15:
	lw	a5,-44(s0)
	addiw	a5,a5,-1
	sext.w	a5,a5
	ld	a4,-64(s0)
	ld	a3,-72(s0)
	ld	a2,-56(s0)
	mv	a1,a5
	ld	a0,-40(s0)
	call	towers_solve_h
	ld	a4,-72(s0)
	ld	a3,-64(s0)
	ld	a2,-56(s0)
	li	a1,1
	ld	a0,-40(s0)
	call	towers_solve_h
	lw	a5,-44(s0)
	addiw	a5,a5,-1
	sext.w	a5,a5
	ld	a4,-72(s0)
	ld	a3,-56(s0)
	ld	a2,-64(s0)
	mv	a1,a5
	ld	a0,-40(s0)
	call	towers_solve_h
.L17:
	nop
	ld	ra,72(sp)
	ld	s0,64(sp)
	addi	sp,sp,80
	jr	ra
	.size	towers_solve_h, .-towers_solve_h
	.align	2
	.globl	towers_solve
	.type	towers_solve, @function
towers_solve:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	addi	s0,sp,32
	sd	a0,-24(s0)
	ld	a5,-24(s0)
	lw	a1,0(a5)
	ld	a5,-24(s0)
	addi	a2,a5,8
	ld	a5,-24(s0)
	addi	a3,a5,24
	ld	a5,-24(s0)
	addi	a5,a5,40
	mv	a4,a5
	ld	a0,-24(s0)
	call	towers_solve_h
	nop
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	jr	ra
	.size	towers_solve, .-towers_solve
	.align	2
	.globl	towers_verify
	.type	towers_verify, @function
towers_verify:
	addi	sp,sp,-48
	sd	ra,40(sp)
	sd	s0,32(sp)
	addi	s0,sp,48
	sd	a0,-40(s0)
	sw	zero,-28(s0)
	ld	a5,-40(s0)
	addi	a5,a5,8
	mv	a0,a5
	call	list_getSize
	mv	a5,a0
	beqz	a5,.L20
	li	a5,2
	j	.L21
.L20:
	ld	a5,-40(s0)
	addi	a5,a5,24
	mv	a0,a5
	call	list_getSize
	mv	a5,a0
	beqz	a5,.L22
	li	a5,3
	j	.L21
.L22:
	ld	a5,-40(s0)
	addi	a5,a5,40
	mv	a0,a5
	call	list_getSize
	mv	a5,a0
	mv	a4,a5
	ld	a5,-40(s0)
	lw	a5,0(a5)
	beq	a4,a5,.L23
	li	a5,4
	j	.L21
.L23:
	ld	a5,-40(s0)
	ld	a5,48(a5)
	sd	a5,-24(s0)
	j	.L24
.L26:
	lw	a5,-28(s0)
	addiw	a5,a5,1
	sw	a5,-28(s0)
	ld	a5,-24(s0)
	lw	a4,0(a5)
	lw	a5,-28(s0)
	sext.w	a5,a5
	beq	a5,a4,.L25
	li	a5,5
	j	.L21
.L25:
	ld	a5,-24(s0)
	ld	a5,8(a5)
	sd	a5,-24(s0)
.L24:
	ld	a5,-24(s0)
	bnez	a5,.L26
	ld	a5,-40(s0)
	lw	a3,4(a5)
	ld	a5,-40(s0)
	lw	a5,0(a5)
	li	a4,1
	sllw	a5,a4,a5
	sext.w	a5,a5
	addiw	a5,a5,-1
	sext.w	a5,a5
	mv	a4,a3
	beq	a4,a5,.L27
	li	a5,6
	j	.L21
.L27:
	li	a5,0
.L21:
	mv	a0,a5
	ld	ra,40(sp)
	ld	s0,32(sp)
	addi	sp,sp,48
	jr	ra
	.size	towers_verify, .-towers_verify
	.align	2
	.globl	main
	.type	main, @function
main:
	addi	sp,sp,-96
	sd	ra,88(sp)
	sd	s0,80(sp)
	addi	s0,sp,96
	mv	a5,a0
	sd	a1,-96(s0)
	sw	a5,-84(s0)
	lui	a5,%hi(g_nodeFreeList)
	addi	a0,a5,%lo(g_nodeFreeList)
	call	list_init
	lui	a5,%hi(g_nodeFreeList)
	addi	a5,a5,%lo(g_nodeFreeList)
	lui	a4,%hi(g_nodePool)
	addi	a4,a4,%lo(g_nodePool)
	sd	a4,8(a5)
	lui	a5,%hi(g_nodeFreeList)
	li	a4,7
	sw	a4,%lo(g_nodeFreeList)(a5)
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	sd	zero,104(a5)
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	li	a4,99
	sw	a4,96(a5)
	sw	zero,-20(s0)
	j	.L29
.L30:
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sext.w	a5,a5
	slli	a4,a5,4
	lui	a5,%hi(g_nodePool)
	addi	a5,a5,%lo(g_nodePool)
	add	a4,a4,a5
	lui	a3,%hi(g_nodePool)
	lw	a5,-20(s0)
	addi	a3,a3,%lo(g_nodePool)
	slli	a5,a5,4
	add	a5,a3,a5
	sd	a4,8(a5)
	lui	a5,%hi(g_nodePool)
	lw	a4,-20(s0)
	slli	a4,a4,4
	addi	a5,a5,%lo(g_nodePool)
	add	a5,a4,a5
	lw	a4,-20(s0)
	sw	a4,0(a5)
	lw	a5,-20(s0)
	addiw	a5,a5,1
	sw	a5,-20(s0)
.L29:
	lw	a5,-20(s0)
	sext.w	a4,a5
	li	a5,5
	ble	a4,a5,.L30
	addi	a5,s0,-80
	li	a1,7
	mv	a0,a5
	call	towers_init
	addi	a5,s0,-80
	mv	a0,a5
	call	towers_clear
	addi	a5,s0,-80
	mv	a0,a5
	call	towers_solve
	addi	a5,s0,-80
	mv	a0,a5
	call	towers_verify
	mv	a5,a0
	mv	a0,a5
	ld	ra,88(sp)
	ld	s0,80(sp)
	addi	sp,sp,96
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.2.0"
