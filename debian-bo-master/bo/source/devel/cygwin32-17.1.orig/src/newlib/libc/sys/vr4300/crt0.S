/*
 * crt0.S -- startup file for MIPS.
 *
 * Copyright (c) 1995 Cygnus Support
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

#define zero	$0
#define v0	$2
#define v1	$3
#define t0	$8
#define t1	$9
#define t2	$10
#define gp	$28
#define sp 	$29
#define fp0	$f0
#define fp1	$f1

/*
 * Set up some room for a stack. We just grab a chunk of memory.
 */
#define STACK_SIZE  0x4000
#define GLOBAL_SIZE 0x2000

        .comm   stack,   STACK_SIZE
	.comm	environ, 0x1
	.comm	globals, GLOBAL_SIZE 

	.text
	.align	2
	.globl	_exit
	.globl	start
	.globl	_start
	.globl	__start

	.ent	start
start:
_start:
__start:
	.set	noreorder
#define SR_CU1		0x20000000
#define SR_PE		0x00100000
#define SR_DE		0x00010000
#define SR_FR		0x04000000
#define C0_SR		$12
#define C0_CAUSE	$13
	li	v0, SR_CU1|SR_DE|SR_FR
	mtc0	v0, C0_SR
	nop
	mtc0	zero, C0_CAUSE
	nop

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
	li	v0, SR_PE|SR_DE|SR_FR
	mtc0	v0, C0_SR
2:

/* Setup memory */
	la	v0, _fbss
	la	v1, _end
bssloop:
	sw	zero,0(v0)
	bne	v0,v1,bssloop
	addiu	v0,v0,4				# executed in delay slot

	la	sp, stack			# set stack pointer
	addiu	sp, STACK_SIZE			# to the top of the allocation
	la	gp, _gp				# set the global data pointer (executed in delay slot)
	jal	main				# call the program start function
	nop
	.end start

/*
 * _exit -- Exit from the application. Normally we cause a user trap
 *          to return to the ROM monitor for another run.
 */
	.ent _exit
_exit:
	nop
	break	0
	j	start				# go back to the beginning
	.end _exit

/* Provide named functions for entry into the monitor: */
#define INDIRECT(name,index)			\
	.globl	name;				\
	.ent	name;				\
name:	la	$2,+(0xbfc00000+((index)*8));	\
	j	$2;				\
	.end name

/* The following magic numbers are for the slots into the IDT monitor: */
INDIRECT(open,6)
INDIRECT(read,7)
INDIRECT(write,8)
INDIRECT(close,10)
INDIRECT(inbyte,11)
INDIRECT(outbyte,12)
INDIRECT(mon_printf,16)
INDIRECT(get_mem_info,55)	/* expects pointer to three word vector */

/* EOF crt0.S */
