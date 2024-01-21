	.globl	_chkr_sigtramp
_chkr_sigtramp:
	save	%sp, -256, %sp
	ld	[ %fp + 0x48 ], %o2	! sigcontext
	sethi	%hi(0x1000), %l0
	ld	[ %o2 + 0x14 ], %o0	! psr
	rd	%y, %l1
	btst	%o0, %l0		! FPU enable
	be	1f
	ld	[%fp + 0x40], %o0	! signal
	cmp	%o0, 8		! 8 == SIG_FPE
	be,a	1f
	clr	%l0
	std	%f0, [ %sp + 0x60 ]
	std	%f2, [ %sp + 0x68 ]
	std	%f4, [ %sp + 0x70 ]
	std	%f6, [ %sp + 0x78 ]
	std	%f8, [ %sp + 0x80 ]
	std	%f10, [ %sp + 0x88 ]
	std	%f12, [ %sp + 0x90 ]
	std	%f14, [ %sp + 0x98 ]
	std	%f16, [ %sp + 0xa0 ]
	std	%f18, [ %sp + 0xa8 ]
	std	%f20, [ %sp + 0xb0 ]
	std	%f22, [ %sp + 0xb8 ]
	std	%f24, [ %sp + 0xc0 ]
	std	%f26, [ %sp + 0xc8 ]
	std	%f28, [ %sp + 0xd0 ]
	std	%f30, [ %sp + 0xd8 ]
	st	%fsr, [ %sp + 0xe0 ]
1:	st	%l1, [ %sp + 0xe4 ]
	std	%g2, [ %sp + 0xe8 ]
	std	%g4, [ %sp + 0xf0 ]
	std	%g6, [ %sp + 0xf8 ]
#ifdef __PIC__
	nop
3:	call	4f
	nop
4:	sethi	%hi(__GLOBAL_OFFSET_TABLE_-(3b-.)),%l7
	or	%l7,%lo(__GLOBAL_OFFSET_TABLE_-(3b-.)),%l7
	add	%l7,%o7,%l7
	ld	[%l7+_chkr_errno],%g1
	mov	%g1, %g1
#else
	sethi	%hi(_chkr_sigfunc), %g1
	or	%g1, %lo(_chkr_sigfunc), %g1
#endif
	ld	[ %fp + 0x44 ], %o1		! code
	sll	%o0, 2, %g2
	ld	[ %g1 + %g2 ], %g1
	call	%g1
	ld	[ %fp + 0x4c ], %o3		! addr
	ld	[ %fp + 0x48 ], %i0
	ld	[ %sp + 0xe4 ], %l1
	ld	[ %i0 + 0x14 ], %o0
	mov	%l1, %y
	btst	%o0, %l0
	be	2f
	ldd	[ %sp + 0xe8 ], %g2
	ldd	[ %sp + 0x60 ], %f0
	ldd	[ %sp + 0x68 ], %f2
	ldd	[ %sp + 0x70 ], %f4
	ldd	[ %sp + 0x78 ], %f6
	ldd	[ %sp + 0x80 ], %f8
	ldd	[ %sp + 0x88 ], %f10
	ldd	[ %sp + 0x90 ], %f12
	ldd	[ %sp + 0x98 ], %f14
	ldd	[ %sp + 0xa0 ], %f16
	ldd	[ %sp + 0xa8 ], %f18
	ldd	[ %sp + 0xb0 ], %f20
	ldd	[ %sp + 0xb8 ], %f22
	ldd	[ %sp + 0xc0 ], %f24
	ldd	[ %sp + 0xc8 ], %f26
	ldd	[ %sp + 0xd0 ], %f28
	ldd	[ %sp + 0xd8 ], %f30
	ld	[ %sp + 0xe0 ], %fsr
2:	ldd	[ %sp + 0xf0 ], %g4
	ldd	[ %sp + 0xf8 ], %g6
	restore	%g0, 0x8b, %g1
	ta	0
	nop
