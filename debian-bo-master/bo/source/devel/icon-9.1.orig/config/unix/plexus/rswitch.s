|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|
| rswitch.s - handle co-expression context switching on a Plexus P-60 or P-35
|	under System V.
|
|	Due to the fact that Sys V is ditributed by Plexus with virtually no
|	documentation on the C-compiler, I'm letting paranoia get the best of
|	me and I'm saving ALL the registers, even though I'm pretty sure that
|	this isn't necessary.
|
|	A. Wingo 8/5/86
|	Rainin Research
|
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
	.file	"rswitch.s"
	.text
	.even
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
|
| coswitch(old_cs, new_cs, first)
|	int *old_cs, *new_cs;
|	int first
|
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

| define argument offsets
OldCs=8
NewCs=12
First=16

|other useful constants
FrameSize = 56
RegMask = 16383

	.globl	_coswitch
_coswitch:
	link	a6,#-FrameSize-4
	moveml	#RegMask,a6@(-FrameSize) |save all regs except a6 & sp in frame
	movl	a6@(OldCs),a0	|save a6 and sp in old context block
	movl	sp,a0@
	movl	a6,a0@(4)

	tstl	a6@(First)	|Is this first call?
	bne	NotFirst	|branch if not

	movl	a6@(NewCs),a0	|else, set stack ptr from new context block
	movl	a0@,sp
	clrl	sp@		| and call new_context
	clrl	sp@-
	jsr	_new_context
	addql	#4,sp
	movl	#Emsg,sp@	|If we get here, something bad happened
	jsr	_syserr		|so go tell user
	bra 	GoHome

NotFirst:
	movl	a6@(NewCs),a0	|If not first invokation, just restore stack
	movl	a0@,sp		|  and frame pointer from new context block
	movl	a0@(4),a6

GoHome:
	moveml	a6@(-FrameSize),#RegMask |Restore all the registers
	unlk	a6
	rts

	.data
Emsg:
	.asciz	"new_context() returned from coswitch"
