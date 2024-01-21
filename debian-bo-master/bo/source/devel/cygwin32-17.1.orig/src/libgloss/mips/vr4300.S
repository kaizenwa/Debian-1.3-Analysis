/*
 * vr4300.S -- CPU specific support routines
 *
 * Copyright (c) 1995,1996 Cygnus Support
 *
 * The authors hereby grant permission to use, copy, modify, distribute,
 * and license this software and its documentation for any purpose, provided
 * that existing copyright notices are retained in all copies and that this
 * notice is included verbatim in any distributions. No written agreement,
 * license, or royalty fee is required for any of the authorized uses.
 * Modifications to this software may be copyrighted by their authors
 * and need not follow the licensing terms described here, provided that
 * the new terms are clearly indicated on the first page of each file where
 * they apply.
 */

#include "regs.S"

	.text
	.align	2

	# Taken from "R4300 Preliminary RISC Processor Specification
	# Revision 2.0 January 1995" page 39: "The Count
	# register... increments at a constant rate... at one-half the
	# PClock speed."
	# We can use this fact to provide small polled delays.
	.globl	__cpu_timer_poll
	.ent	__cpu_timer_poll
__cpu_timer_poll:
	.set	noreorder
	# in:	a0 = (unsigned int) number of PClock ticks to wait for
	# out:	void

	# The Vr4300 counter updates at half PClock, so divide by 2 to
	# get counter delta:
	bnezl	a0, 1f		# continue if delta non-zero
	srl	a0, a0, 1	# divide ticks by 2		{DELAY SLOT}
	# perform a quick return to the caller:
	j	ra
	nop			#				{DELAY SLOT}
1:
	mfc0	v0, $9		# C0_COUNT:  get current counter value
	nop
	nop
	# We cannot just do the simple test, of adding our delta onto
	# the current value (ignoring overflow) and then checking for
	# equality. The counter is incrementing every two PClocks,
	# which means the counter value can change between
	# instructions, making it hard to sample at the exact value
	# desired.

	# However, we do know that our entry delta value is less than
	# half the number space (since we divide by 2 on entry). This
	# means we can use a difference in signs to indicate timer
	# overflow.
	addu	a0, v0, a0	# unsigned add (ignore overflow)
	# We know have our end value (which will have been
	# sign-extended to fill the 64bit register value).
2:
	# get current counter value:
	mfc0	v0, $9	# C0_COUNT
	nop
	nop
	# This is an unsigned 32bit subtraction:
	subu	v0, a0, v0	# delta = (end - now)		{DELAY SLOT}
	bgtzl	v0, 2b		# looping back is most likely
	nop
	# We have now been delayed (in the foreground) for AT LEAST
	# the required number of counter ticks.
	j	ra		# return to caller
	nop			#				{DELAY SLOT}
	.set	reorder
	.end	__cpu_timer_poll

	# Flush the processor caches to memory:

	.globl	__cpu_flush
	.ent	__cpu_flush
__cpu_flush:
	.set	noreorder
	# NOTE: The Vr4300 *CANNOT* have any secondary cache (bit 17
	# of the CONFIG registered is hard-wired to 1). We just
	# provide code to flush the Data and Instruction caches.

	# Even though the Vr4300 has hard-wired cache and cache line
	# sizes, we still interpret the relevant Config register
	# bits. This allows this code to be used for other conforming
	# MIPS architectures if desired.

	# Get the config register
	mfc0	a0, C0_CONFIG
	nop
	nop
	li	a1, 1		# a useful constant
	#
	srl	a2, a0, 9	# bits 11..9 for instruction cache size
	andi	a2, a2, 0x7	# 3bits of information
	add	a2, a2, 12	# get full power-of-2 value
	sllv	a2, a1, a2	# instruction cache size
	#
	srl	a3, a0, 6	# bits 8..6 for data cache size
	andi	a3, a3, 0x7	# 3bits of information
	add	a3, a3, 12	# get full power-of-2 value
	sllv	a3, a1, a3	# data cache size
	#
	li	a1, (1 << 5)	# check IB (instruction cache line size)
	and	a1, a0, a1	# mask against the CONFIG register value
	beqz	a1, 1f		# branch on result of delay slot operation
	nop
	li	a1, 32		# non-zero, then 32bytes
	j	2f		# continue
	nop
1:
	li	a1, 16		# 16bytes
2:
	#
	li	t0, (1 << 4)	# check DB (data cache line size)
	and	a0, a0, t0	# mask against the CONFIG register value
	beqz	a0, 3f		# branch on result of delay slot operation
	nop
	li	a0, 32		# non-zero, then 32bytes
	j	4f		# continue
	nop
3:
	li	a0, 16		# 16bytes
4:
	#
	# a0 = data cache line size
	# a1 = instruction cache line size
	# a2 = instruction cache size
	# a3 = data cache size
	#
	li	t0, K0BASE
	addu	t1, t0, a2	# end cache address
	subu	t2, a1, 1	# line size mask
	not	t2		# invert the mask
	and	t3, t0, t2	# get start address
	addu	t1, -1
	and	t1, t2		# get end address
5:
	cache	INDEX_INVALIDATE_I,0(t3)
	bne	t3, t1, 5b
	addu	t3, a1
	#
	addu	t1, t0, a3	# end cache address
	subu	t2, a0, 1	# line size mask
	not	t2		# invert the mask
	and	t3, t0, t2	# get start address
	addu	t1, -1
	and	t1, t2		# get end address
6:
	cache	INDEX_WRITEBACK_INVALIDATE_D,0(t3)
	bne	t3, t1, 6b
	addu	t3, a0
	#
	j	ra	# return to the caller
	nop
	.set	reorder
	.end	__cpu_flush

/* EOF vr4300.S */
