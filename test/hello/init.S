	.text
	.align	1

        .globl _start
        .type _start, @function
_start:
        la      a0,trap_handler
        csrw    mtvec,a0
        jal     main

	.globl	trap_handler
	.type	trap_handler, @function
trap_handler:
        csrr    t5,mepc
        addi    t5,t5,4
        csrw    mepc,t5
        li      t5,64
        beq     a7,t5,handle_write
exit_trap_handler:   
        mret
handle_write:
        bne     a0,x0,exit_trap_handler
handle_write_loop:      
        beq     a2,x0,exit_trap_handler
        lb      t5,0(a1)
        sb      t5,100(zero)
        addi    a1,a1,1
        addi    a2,a2,-1
        j       handle_write_loop
	.size	trap_handler, .-trap_handler
	.ident	"GCC: (GNU) 8.2.0"
