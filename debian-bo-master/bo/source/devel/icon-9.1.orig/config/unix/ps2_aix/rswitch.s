/
/ This is the co-expression context switch for the Intel 80386
/ under PS/2 AIX.
/

/
/ coswitch
/
	.extern	coswitch,syserr,new_context

coswitch:

	pushl	%ebp			/ Create stack frame
	movl	%esp,%ebp		/ ...

	movl	8(%ebp),%eax		/ get pointer to old_cs
	movl	%esp,(%eax)		/ save registers in it
	movl	%ebp,4(%eax)		/ ...
	movl	%ebx,8(%eax)		/ ...
	movl	%esi,12(%eax)		/ ...
	movl	%edi,16(%eax)		/ ...

	movl	12(%ebp),%eax		/ get pointer to new_cs

	cmpl	$0,16(%ebp)		/ test first-time flag
	jnz	1f			/ jump if not first activation

	/
	/ First activation
	/
	movl	(%eax),%esp		/ get new stack pointer
	movl	$0,%ebp			/ clear fp
	/
	/ Call C function new_context(0,0).
	/
	pushl	$0			/ push 0,0
	pushl	$0			/ ...
	call	new_context			/ call new_context
	/ addl	$8,%esp			/ pop stack (not really needed here)
	/
	/ Call C function: syserr("function returned in coswitch").
	/
	pushl	$msg			/ push pointer to msg
	call	syserr			/ call syserr (doesn't return)

1:
	/
	/ Non-first activation.
	/
	movl	(%eax),%esp		/ restore regs from new_cs
	movl	4(%eax),%ebp		/ ...
	movl	8(%eax),%ebx		/ ...
	movl	12(%eax),%esi		/ ...
	movl	16(%eax),%edi		/ ...
	leave				/
	ret				/ return

	.data
msg:	.string	"new_context() returned in coswitch"
