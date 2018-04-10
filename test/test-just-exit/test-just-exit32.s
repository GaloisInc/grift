	.file	"test-just-exit.c"
	.option nopic
	.text
	.align	2
	.globl	foo, _start
	.type	foo, @function
_start:
	addi	sp,zero,0x0
	lui	sp,0x1
	call main
	ecall
foo:
	add	sp,sp,-16
	sw	s0,12(sp)
	add	s0,sp,16
	li	a5,171
	mv	a0,a5
	lw	s0,12(sp)
	add	sp,sp,16
	jr	ra
	.size	foo, .-foo
	.align	2
	.globl	main
	.type	main, @function
main:
	add	sp,sp,-32
	sw	ra,28(sp)
	sw	s0,24(sp)
	add	s0,sp,32
	call	foo
	sw	a0,-20(s0)
	lw	a5,-20(s0)
	mv	a0,a5
	lw	ra,28(sp)
	lw	s0,24(sp)
	add	sp,sp,32
	jr	ra
	.size	main, .-main
	.ident	"GCC: (GNU) 7.1.1 20170509"
