/*
 * Assembler source for context switch using gas 1.38.1 + gcc 1.40 on
 * Xenix/386, updated dev. sys. version 2.3.0, kernel version 2.3.3.
 * Should work for any Xenix/386 system that has gas+gcc.
 */


.file	"rswitch.s"
.data 1
.LC0:
	.byte 0x6e,0x65,0x77,0x5f,0x63,0x6f,0x6e,0x74,0x65,0x78
	.byte 0x74,0x28,0x29,0x20,0x72,0x65,0x74,0x75,0x72,0x6e
	.byte 0x65,0x64,0x20,0x69,0x6e,0x20,0x63,0x6f,0x73,0x77
	.byte 0x69,0x74,0x63,0x68,0x0
.text
	.align 2
.globl _coswitch


_coswitch:
	pushl %ebp		/* save current stack position */
	movl %esp,%ebp
	movl 8(%ebp),%eax	/* move old_cs into eax */
	movl %esp,0(%eax)	/* save the current sp where old_cs points */
	movl %ebp,4(%eax)	/* save base pointer as well */
	movl 12(%ebp),%eax	/* now, move new_cs into eax*/
	cmpl $0,16(%ebp)	/* if the 3rd arg to _coswitch equals 0... */
	movl 0(%eax),%esp	/* make new_cs the new stack context */
	je .L2			/* ...then it's the first activation */

/* case *not* first activation */
	movl 4(%eax),%ebp	/* restore saved base pointer */
	jmp .L1

/* case first activation */
.L2:
	movl $0,%ebp
	pushl $0
	pushl $0
	call _new_context	/* create new context for coexp */
	pushl $.LC0
	call _syserr
	addl $12,%esp		/* restore stack */

/* clean up and go home to mother */
.L1:
	leave
	ret
