	.file	"getctxt.c"
	.section ".text"
	.align 4

.Ltext0:
	.globl chkr_getcontext
chkr_getcontext:
	.type	chkr_getcontext,#function
	save  %sp, -96, %sp
	mov  15, %o0			! uc_flags = 15
	st  %o0, [ %i0 ]
	call  chkr___getcontext
	mov  %i0, %o0			! chkr___getcontext (uc)
	tst  %o0
	be,a   .L0
	add  40, %i0, %i0
	b  .L1
	mov  -1, %i0
.L0:
	! %i0 points to uc_mcontext
	add  %g0, %sp, %o0
	ld  [ %o0 + 60 ], %o2		! saved pc
	st  %o2, [ %i0 + 72 ]		! -> REG_O7	???
	add  8, %o2, %o2
	st  %o2, [ %i0 + 4 ]		! saved_pc + 8 -> REG_PC
	add  4, %o2, %o2
	st  %o2, [ %i0 + 8 ]		! saved_pc + 12 -> REG_nPC
	clr  [ %i0 + 44 ]		! REG_O0
	ld  [ %o0 + 56 ], %o2		! saved_fp
	st  %o2, [ %i0 + 68 ]		! -> REG_O6
	mov  0, %i0
.L1:
	ret 
	restore 
