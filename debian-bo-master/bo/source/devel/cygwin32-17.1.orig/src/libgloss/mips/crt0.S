/*
 * crt0.S -- startup file for MIPS.
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

/*
 * Set up some room for a stack. We just grab a chunk of memory.
 */
#define STACK_SIZE  0x4000
#define GLOBAL_SIZE 0x2000

	.comm	__memsize, 12
	.comm	__lstack, 48
	.comm	_gp,4

	.text
	.align	2
	.globl	_start
	.ent	_start
_start:
	.set	noreorder
	li	v0, SR_CU1|SR_PE|SR_FR
	mtc0	v0, C0_SR
	mtc0	zero, C0_CAUSE

/* Check for FPU presence */
	li	t2,0xAAAA5555
	mtc1	t2,fp0		/* write to FPR 0 */
	mtc1	zero,fp1	/* write to FPR 1 */
	mfc1	t0,fp0
	mfc1	t1,fp1
	nop
	bne	t0,t2,1f	/* check for match */
	nop
	bne	t1,zero,1f	/* double check */
	nop
	j	2f		/* status register already correct  */
	nop
1:
	li	v0, SR_PE|SR_FR
	mtc0	v0, C0_SR
2:

/*
 * zero out the bss section.
 */
	.globl	__memsize
	.globl	get_mem_size
	.globl  _gp
	.globl	__stack
	.globl	__global
	.globl	zerobss
zerobss:
	la	v0, _fbss
	la	v1, _end
3:
	sw	zero,0(v0)
	bne	v0,v1,3b
	addiu	v0,v0,4				# executed in delay slot

	la	t0, __lstack			# make a small stack so we
	move	sp, t0				# can run some C code
	la	a0, __memsize			# get the usable memory size
	jal	get_mem_info
	nop

	/* setup the stack pointer */
	la	t0,__stack			# is __stack set ?
	bne	t0,zero,4f
	nop

	/* NOTE: a0[0] contains the amount of memory available, and
	         not the last memory address. */
	lw	t0,0(a0)			# last address of memory available
	la	t1,K0BASE			# cached kernel memory
	addu	t0,t0,t1			# get the end of memory address
	sub	t0,t0,8				# and generate a starting stack-pointer
4:
	move	sp,t0				# set stack pointer

	/* setup the global pointer */
	la	t2,__global			# is __global set ?
	bne	t2,zero,5f
	nop
	sub	t2,t0,STACK_SIZE-8		# get some more memory
5:
	move	gp,t2				# set the global data pointer
	sw	gp,_gp				# set a symbol that ld can use

/*
 * initialize target specific stuff. Only execute these
 * functions it they exist.
 */
	.globl	hardware_init_hook
	.globl	software_init_hook
	.globl	__do_global_dtors
/*	.globl	atexit */
	.globl	exit
	.globl	init
init:
	la	t0,hardware_init_hook		# init the hardware if needed
	beq	t0,zero,6f
	nop
	jal	hardware_init_hook
	nop
6:
	la	t1,software_init_hook		# init the software if needed
	beq	t1,zero,7f
	nop
	jal	software_init_hook
	nop
7:
	.globl	__do_global_dtors
	la	a0,__do_global_dtors
/*	jal	atexit	*/
	nop

	jal	main				# call the program start function
	nop

	# fall through to the "exit" and _exit routines
	.end _start

	jal	exit				# call libc exit to run the G++
	nop					# destructors
	
/*
 * _exit -- Exit from the application. Normally we cause a user trap
 *          to return to the ROM monitor for another run. NOTE: This is
 *	    the only other routine we provide in the crt0.o object, since
 *          it may be tied to the "_start" routine. It also allows
 *          executables that contain a complete world to be linked with
 *          just the crt0.o object.
 */
	.globl	_exit
	.ent _exit
_exit:
7:	
	break	0
	nop
	b	7b				# but loop back just in-case
	nop
	.end _exit

/* EOF crt0.S */
