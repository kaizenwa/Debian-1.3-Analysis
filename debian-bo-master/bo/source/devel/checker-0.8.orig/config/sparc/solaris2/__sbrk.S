! TODO: write this file in C
	.file	"sbrk.S"

	.section ".data"
	.align 8
_sbrk_addr:
	.type _sbrk_addr,#object
	.long end
	.long 0

	.section ".text"
	.align 4

	.globl chkr_sbrk
chkr_sbrk:
	.type	chkr_sbrk,#function
	add  7, %o0, %o0		! round
	andn  %o0, 7, %o0
	sethi  %hi(_sbrk_addr), %o2
	ld  [ %o2 + %lo(_sbrk_addr) ], %o3
	add  7, %o3, %o3		! round
	andn  %o3, 7, %o3
	add  %o3, %o0, %o0		! add
	mov  %o0, %o4
	mov  17, %g1
	ta  8
	bcc  .L1
	sethi  %hi(chkr_errno), %o5
	st %o0, [%o5 + %lo(chkr_errno)]
	b .L11
	mov -1, %o0
.L1:
	st  %o4, [ %o2 + %lo(_sbrk_addr) ]
	mov  %o3, %o0
.L11:
	retl
	nop

	.globl chkr_brk
chkr_brk:
	.type chkr_brk,#function
	add  7, %o0, %o0
	andn  %o0, 7, %o0
	mov  %o0, %o2
	mov  17, %g1
	ta  8
	bcc  .L2
	sethi  %hi(chkr_errno), %o5
	st %o0, [%o5 + %lo(chkr_errno)]
	b .L21
	mov -1, %o0
.L2:
	sethi  %hi(_sbrk_addr), %g1
	st  %o2, [ %g1 + %lo(_sbrk_addr) ]
	mov  %g0, %o0
.L21:
	retl
	nop

	.globl chkr_init_sbrk
chkr_init_sbrk:
	.type chkr_init_sbrk,#function
	retl
	nop
