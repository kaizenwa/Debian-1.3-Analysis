# rover.s for Unistride 2.1 on Stride 400 series.
#
# This file is copied from the one supplied by Tom Mitchell
# for Icon v6 (90l12;rdf); apparently written by Bruce __.
	global	runerr
	global	ckadd
	global	cksub
	global	ckmul


	text

ckadd:	mov.l	4(%sp),%d0	# Perform addition
	add.l	8(%sp),%d0	#  " " "
	bvs.b	oflow		# Branch if overflow
	rts			# Return result in %d0

cksub:	mov.l	4(%sp),%d0	# Perform subtraction
	sub.l	8(%sp),%d0	#  " " "
	bvs.b	oflow		# Branch if overflow
	rts			# Return result in %d0


ckmul:	tst.w	4(%sp)		# jump if operand 1 is > 65535
	bne.b	L%mul
	tst.w	8(%sp)		# jump if operand 2 is > 65535
	bne.b	L%mul
	mov.w	6(%sp),%d0	# do a fast multiply if both are small
	mulu.w	10(%sp),%d0
	bvs.b	oflow		# Branch if overflow
	rts			# Return result in %d0

L%mul:	mov.l	4(%sp),%d0	# slow multiply required...
	mov.l	%d2,%a0		# this routine stolen from the C library
	mov.l	%d3,%a1		# then modified to check for overflows 
	mov.w	%d0,%d2		# which "c" could care less about.
	mov.w	%d0,%d1
	ext.l	%d1
	swap.w	%d1
	swap.w	%d0
	sub.w	%d0,%d1
	bvs.b	oflow
	mov.w	10(%sp),%d0
	mov.w	%d0,%d3
	ext.l	%d3
	swap.w	%d3
	sub.w	8(%sp),%d3
	bvs.b	oflow
	muls.w	%d0,%d1
	bvs.b	oflow
	muls.w	%d2,%d3
	bvs.b	oflow
	add.w	%d1,%d3
	bvs.b	oflow
	muls.w	%d2,%d0
	bvs.b	oflow
	swap.w	%d0
	sub.w	%d3,%d0
	bvs.b	oflow
	swap.w	%d0
	mov.l	%a0,%d2
	mov.l	%a1,%d3
	rts			# Return result in %d0


oflow:				# Got overflow on an operation
	pea.l	0
	pea.l	203
	jsr	runerr		# runerr(203,0)
# no return!
