/*
 * pmon.S -- lo-level entry points into PMON monitor.
 *
 * Copyright (c) 1996 Cygnus Support
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


/* Provide named functions for entry into the IDT monitor: */
#define INDIRECT(name,index)			\
	.globl	name;				\
	.ent	name;				\
	.set	noreorder;			\
name:	la	$2,+(0xbfc00500+((index)*4));	\
	lw	$2,0($2);			\
	j	$2;				\
	nop;					\
	.set	reorder;			\
	.end	name

/* The following magic numbers are for the slots into the PMON monitor */
/* The first are used as the lo-level library run-time: */
INDIRECT(read,0)
INDIRECT(write,1)
INDIRECT(open,2)
INDIRECT(close,3)
/* The following are useful monitor routines: */
INDIRECT(mon_ioctl,4)
INDIRECT(mon_printf,5)
INDIRECT(mon_vsprintf,6)
INDIRECT(mon_ttctl,7)
INDIRECT(mon_cliexit,8)
INDIRECT(mon_getenv,9)
INDIRECT(mon_onintr,10)
INDIRECT(mon_flush_cache,11)
INDIRECT(mon_exception,12)

/* The following routine is required by the "print()" function: */
	.globl	outbyte
	.ent	outbyte
	.set	noreorder
outbyte:
	subu	sp,sp,0x20	/* allocate stack space for string */
	sd	ra,0x18(sp)	/* stack return address */
	sd	fp,0x10(sp)	/* stack frame-pointer */
	move	fp,sp		/* take a copy of the stack pointer */
	/* We leave so much space on the stack for the string (16
	   characters), since the call to mon_printf seems to corrupt
	   the 8bytes at offset 8 into the string/stack. */
	sb	a0,0x00(sp)	/* character to print */
	sb	z0,0x01(sp)	/* NUL terminator */
	jal	mon_printf	/* and output the string */
	move	a0,sp		/* take a copy of the string pointer {DELAY SLOT} */

	move	sp,fp		/* recover stack pointer */
	ld	ra,0x18(sp)	/* recover return address */
	ld	fp,0x10(sp)	/* recover frame-pointer */
	j	ra		/* return to the caller */
	addu	sp,sp,0x20	/* dump the stack space {DELAY SLOT} */
	.set	reorder
	.end	outbyte

/* The following routine is required by the "sbrk()" function: */
	.globl	get_mem_info
	.ent	get_mem_info
	.set	noreorder
get_mem_info:
	# in:  a0 = pointer to 3 word structure
	# out: void
	subu	sp,sp,0x18	/* create some stack space */
	sd	ra,0x00(sp)	/* stack return address */
	sd	fp,0x08(sp)	/* stack frame-pointer */
	sd	a0,0x10(sp)	/* stack structure pointer */
	move	fp,sp		/* take a copy of the stack pointer */

	# The monitor has already sized memory, but unfortunately we
	# do not have access to the data location containing the
	# memory size.

	jal	__sizemem
	nop

	ld	a0,0x10(sp)	# recover structure pointer
	sw	v0,0(a0)	# amount of memory available

	# Deal with getting the cache size information:
	mfc0	a1, C0_CONFIG
	nop
	nop
	andi	a2,a1,0x7 << 9	# bits 11..9 for instruction cache size
	sll	a2,a2,12 - 8
	sw 	a2,4(a0)
	andi	a2,a1,0x7 << 6	# bits 8..6 for data cache size
	sll	a2,a2,12 - 5
	sw 	a2,8(a0)	# data cache size
	#
	move	sp,fp		/* recover stack pointer */
	ld	ra,0x00(sp)	/* recover return address */
	ld	fp,0x08(sp)	/* recover frame-pointer */
	j	ra		/* return to the caller */
	addu	sp,sp,0x18	/* restore stack pointer {DELAY SLOT} */
	.set	reorder
	.end	get_mem_info

/* EOF pmon.S */
