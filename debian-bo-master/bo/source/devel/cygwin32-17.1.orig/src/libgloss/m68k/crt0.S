/*
 * crt0.S -- startup file for m68k-coff
 *
 * Copyright (c) 1995, 1996 Cygnus Support
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

#include "asm.h"

	.title "crt0.S for m68k-coff"
#define STACKSIZE	0x4000

/*
 * Define an empty environment.
 */
        .data
        .align 2
SYM (environ):
        .long 0

 	.align	2
	.text

/*
 * These symbols are defined in C code, so they need to always be
 * named with SYM because of the difference between object file formats.
 */
	.global SYM (start)
	.global SYM (zerobss)
	.global SYM (init)
	.global SYM (launch)

/* These are defined in C code. */
	.global SYM (main)
	.global SYM (exit)
	.global SYM (hardware_init_hook)
	.global SYM (software_init_hook)
	.global SYM (atexit)
	.global SYM(__do_global_dtors)

/* 
 * These values are set in the linker script, so they must be
 * explicitly named here without SYM.
 */
	.global __stack
	.global __bss_start
	.global _end

/*
 * set things up so the application will run. This *must* be called start.
 */
SYM (start):
	/*
	 * put any hardware init code here
	 */

	/* see if the stack is already setup. if not, then default
	 *  to using the value of %sp as set by the ROM monitor
	 */
	tstl    IMM(__stack)
	jbne    1f
	moveal  IMM(__stack), sp
1:
	/* set up initial stack frame */
	link	a6, IMM(-8)

/*
 * zero out the bss section.
 */
SYM(zerobss):
	movel	IMM(__bss_start), d1
	movel	IMM(_end), d0
	cmpl	d0, d1
	jbeq	3f
	movl	d1, a0
	subl	d1, d0
2:
	clrb	(a0)+
#ifndef __mcf5200__
	dbra	d0, 2b
	clrw	d0
#endif
	subql	IMM(1), d0
	jbcc	2b
3:

/*
 * initialize target specific stuff. Only execute these
 * functions it they exist.
 */
SYM(init):
	lea	SYM (software_init_hook), a0
	tstl    a0
	jbeq	4f
	jsr     (a0)
4:

	lea	SYM (hardware_init_hook), a1
	tstl    a1
	jbeq	5f
	jsr     (a1)
5:

/*
 * call the main routine from the application to get it going.
 * main (argc, argv, environ)
 * we pass argv as a pointer to NULL.
 */
SYM (launch):

#ifdef ADD_DTORS
	/* put __do_global_dtors in the atexit list so the destructors get run */
	movel	IMM (SYM(__do_global_dtors)),(sp)
	jsr	SYM (atexit)
#endif

        pea     0
        pea     SYM (environ)
        pea     sp@(4)
        pea     0
	jsr	SYM (main)
	movel	d0, sp@-

/*
 * drop down into exit incase the user doesn't. This should drop
 * control back to the ROM monitor, if there is one. This calls the
 * exit() from the C library so the C++ tables get cleaned up right.
 */
        jsr     SYM (exit)
