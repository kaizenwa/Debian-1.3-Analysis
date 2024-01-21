|
|  Co-expression context switch for DIAB D-NIX Icon
|
	.text
	.globl	_coswitch
_coswitch:
|
|  Save registers of the co-expression being deactivated.
|
	movl	4(sp),a0	|Get address of old task's registers
	movl	8(sp),a1	|Get address of new task's registers
	addl	#12*4,a0	|Bump to end of area for backward move
	moveml	#<a2,a3,a4,a5,a6,a7,d2,d3,d4,d5,d6,d7>,-(a0)	|Save registers
	tstl	12(sp)		|Check whether this is first activation
	bnes	not_first	|Jump if not first
|
|  Come here for co-expression's first activation.  Set the stack
|  pointer to its stack and call new_context.
|
first:
	movl	(a1),sp		|Get new stack pointer
	movl	#0,-(sp)	|Set up args for new_context
	movl	#0,-(sp)	| ...
	jbsr	_new_context		|Call new_context
	addqw	#8,sp		|Pop args off stack
|
|  We should never get here, but just in case ...
|
	pea	err_msg		|Address of error message
	jbsr	_syserr		|System error
|
| Come here if not the first activation.  Restore registers and return.
|
not_first:
	moveml	(a1)+,#<a2,a3,a4,a5,a6,a7,d2,d3,d4,d5,d6,d7> |Restore registers
	rts			|Return

	.data
err_msg:
	.ascii	"new_context() returned in coswitch"
	.byte	0
