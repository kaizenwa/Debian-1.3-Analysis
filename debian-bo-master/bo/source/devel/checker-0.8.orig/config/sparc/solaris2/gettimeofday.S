	.file	"gettimeofday.S"
	.section ".text"
	.align 4

	.globl chkr_gettimeofday
chkr_gettimeofday:
	.type	chkr_gettimeofday,#function
	tst  %o1
	be  .L1
	tst  %o0
	st   %g0, [%o1]		! 0 -> *tz
	st   %g0, [%o1 + 4]
.L1:	be  .L0
	mov  %o0, %o5		! save %o0
	ta  39
	st  %o0, [ %o5 ]	! tv_sec
	sra  %o1, 2, %o2	! big *
	add  %o1, %o2, %o2
	srl  %o2, 1, %o2
	add  %o1, %o2, %o2
	srl  %o2, 2, %o2
	add  %o1, %o2, %o2
	srl  %o2, 4, %o2
	add  %o1, %o2, %o2
	srl  %o2, 3, %o2
	sub  %o1, %o2, %o2
	srl  %o2, 2, %o2
	add  %o1, %o2, %o2
	srl  %o2, 3, %o2
	add  %o1, %o2, %o2
	srl  %o2, 4, %o2
	add  %o1, %o2, %o2
	srl  %o2, 1, %o2
	add  %o1, %o2, %o2
	srl  %o2, 6, %o2
	add  %o1, %o2, %o1
	srl  %o1, 10, %o1
	st  %o1, [ %o5 + 4 ]
.L0:
	retl 
	mov  %g0, %o0
