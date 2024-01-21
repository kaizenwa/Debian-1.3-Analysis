/* Stubs which checks the addresses. This code is only called by your
    program. Not portable...
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written September 1993 Tristan Gingold

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License 
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

The author may be reached by US/French mail:
		Tristan Gingold 
		8 rue Parmentier
		F-91120 PALAISEAU
		FRANCE
*/

#include "define.h"

#ifdef __ELF__
#define C_NAME(x) x
#define FUNCTION(name) .type name, @function
#define SIZE(name) .size name, . - name
#else
#define C_NAME(x) _##x
#define FUNCTION(name)
#define SIZE(name)
#endif

/* infos: on Linux, ss = ds = es = fs = gs
 *  Binary constants must begin with "0B", not with "0b" (backward local label)
 */
	.file "codecheck.S"
	.data
	.align 4
/* Saved registers. */
/* Please, keep this order (see Linux/linux-syscall.c and Linux/sigsend.c). */
	.globl C_NAME(linux_context)
C_NAME(linux_context):
Reax:	.long 0
Recx:	.long 0
Redx:	.long 0
Rebx:	.long 0
Resp:	.long 0
Rebp:	.long 0
Resi:	.long 0
Redi:	.long 0	
Reip:	.long 0
Rflag:	.long 0

/* Some values. Keep this order (idem) */
size:	.long 0
right:	.long 0
accmod:	.long 0
addr:	.long 0

	.text
	.align 4
#ifdef CHKR_DEBUG
	.globl save_register, restore_register, Check_addr
# /*	.globl Sib, Chkr_Modrm */
	.globl Reax, Recx, Redx, Rebx, Resp, Rebp, Resi, Redi, Reip
	.globl size, right, accmod
#endif

/* Function to save registers. We can directly access them and call C funcs. */
/*  This function also update known_stack_limit and stack_bitmapped. */
/*  OK, this code is absolutely *not* optimized, but it works. */
/* To optimize: save only registers not saved by GCC */
/* Be very careful with chkr_in_checker: incl/decl changes flags.  */
	FUNCTION(save_register)
save_register:							/* cycles */
	pushf			# /* Save flags */		/* 4/3 */
	incl C_NAME(chkr_in_checker)	# /* Enter in Checker.  */
	popl Rflag						/* 5 */
	cld			# /* Default for gcc */		/* 2 */
	movl %eax, Reax						/* 1 */
	movl %ebx, Rebx
	movl %ecx, Recx
	movl %edx, Redx
	movl %esi, Resi
	movl %edi, Redi
	movl %ebp, Rebp
	leal 8(%esp), %eax	# /* Compute esp */		/* 1 */
	movl %eax, Resp						/* 1 */
	movl %eax, C_NAME(known_stack_limit)	# /* Always updated */	/* 1 */
	cmpl %eax, C_NAME(stack_bitmapped)				/* 2 */
	jge L00							/* 3/1 */
	call C_NAME(adjust_bm_when_stack_reduce)
L00:	movl 4(%esp), %esi		# /* Compute eip */	/* 1 */
	movl %esi, Reip						/* 1 */
	ret							/* 5 */
	SIZE(save_register)

	.align 4
/* Function to restore registers. */
	FUNCTION(restore_register)
restore_register:
	cmpl $0, C_NAME(chkr_sig_catched)
	je L01				# /* no signals to send */
	decl C_NAME(chkr_in_checker)	# /* we leave Checker */
	pushl $C_NAME(linux_context)
	call C_NAME(chkr_send_delayed_signals)	# /* send the signals */
	incl C_NAME(chkr_in_checker)	# /* really useful !! */
	pop %eax			# /* Just to remove linux_context */
L01:	movl Reax, %eax
	movl Rebx, %ebx
	movl Recx, %ecx
	movl Redx, %edx
	movl Resi, %esi
	movl Redi, %edi
	movl Rebp, %ebp
#ifndef CHKR_IS_SAFE
	/* If here we are not in Checker, then this is not a computer :-) */
	cmpl $0, C_NAME(chkr_in_checker)
	je abort
#endif
	pushl Rflag			# /* Restore flags. */
	decl C_NAME(chkr_in_checker)	# /* Leave Checker.  */
	popf
/* eip and esp will be automatically restored */
	ret
	SIZE(restore_register)

#ifdef OLD_CHKR
	.align 4
Chkr_Modrm:
	movb (%esi), %al	# /* Load the ModRm byte into al */ 
	andb $0B11000111, %al	# /* We use only the Mod and Rm fields */
#ifndef CHKR_IS_SAFE
	movb %al, %ah		# /* Is it a register ? */
	andb $0B11000000, %ah
	cmpb $0B11000000, %ah
	je abort 		# /* This is a register. Must never happen */
#endif /* !CHKR_IS_SAFE */
	movb %al, %ah		# /* Test for s-i-b byte */
	andb $0B00000111, %ah
	cmpb $0B00000100, %ah
	je Sib
/* s-i-b byte doesn't follow */
	cmpb $0B00000101, %al	# /* 0b00???101 is special. test it */
	jne L1
/* A 32 disp follows */
	movl 1(%esi),%edi
	jmp Check_addr
/* Load the register */
L1:	movl %eax, %edi
	andl $0B00000111, %edi
	movl registers(,%edi,4), %edi
/* Is there a disp ? */
	movb %al, %ah
	andb $0B11000000, %ah
	cmp  $0B00000000, %ah
	je Check_addr 		# /* no disp, so finish */
	cmp  $0B01000000, %ah	# /* test for mod == 01 */
	jne L2
/* just add a disp8 */
	movsbl 1(%esi), %edx	# /* signed convertion 8 bits to 32 bits */
	addl %edx, %edi
	jmp Check_addr
/* Just add a disp32 */
L2:	addl 1(%esi), %edi
	jmp Check_addr

/* Handle the sib byte 
   In Sib: %al is Modrm, %ah is s-i-b */
Sib:	andb $0B11000000, %al	# /* ignore unuseful bits */
	movb 1(%esi), %ah	# /* load sib byte into ah */
/* handle scale and index */
	movb %ah, %cl
	andb $0B00111000, %cl	# /* mask scale and base */
	cmpb $0B00100000, %cl	# /* test for no index */
	jne L3
	xor  %edi, %edi		# /* clean the address */
	jmp L4			# /* skip scale, handle directlry the disp */
L3:	xor %ecx, %ecx		# /* clean all 32bits of ecx */
	movb %ah, %cl		
	andb $0B00111000, %cl	# /* keep only index */
	shrb $1, %cl		# /* shift -> the index is good */
	movl registers(%ecx), %edi	# /* load the register */
	movb %ah, %cl		
	andb $0B11000000, %cl	# /* mask index and base */
	shrb $6, %cl		# /* move the index into the first 2 bits */
	shll %cl, %edi		# /* scale now */
L4:	andb $0B00000111, %ah	# /* keep only base */
	cmpw $0x0500, %ax	# /* special case: mod=0, base=101. */
	jne L5
	addl 2(%esi), %edi	# /* add the disp32 */
	jmp Check_addr		# /* end */
L5:	xor %ecx, %ecx		# /* clean ecx, for base register */
	movb %ah, %cl		# /* load base into cl */
	addl registers(,%ecx,4), %edi	# /* add the base register */
	cmpb $0B01000000, %al	# /* Is this a disp8 */
	jne L6			# /* not, handle no disp/disp32 */
	movsbl 2(%esi), %edx	# /* convert disp8 to 32 bits */
	addl %edx, %edi		# /* add the disp */
	jmp Check_addr
L6:	cmpb $0B00000000, %al	# /* test for no disp */
	je Check_addr
	addl 2(%esi), %edi	# /* add disp32 */
	jmp Check_addr
#endif /* OLD_CHKR */
	
# /* Check_addr calls chkr_check_addr		*/
# /*         arguments: accmod -> right		*/
# /*			size -> len		*/
# /*			%edi -> ptr		*/
	FUNCTION(Check_addr)
Check_addr:
	movl accmod, %eax	# /* push the "right" arg */
	pushl %eax
	movl size, %eax		# /* push the "len" arg */
	pushl %eax
	pushl %edi		# /* push the "ptr" arg */
	call C_NAME(chkr_check_addr)	# /* call the chkr_check_addr */
	addl $12, %esp		# /* normalize the stack */
	ret
	SIZE(Check_addr)

# /* display a message (unknown instruction) and abort */
	FUNCTION(abort)
abort:
	pushl %ebp		# /* push the frame pointer */
	movl %esp, %ebp
	pushl $1
	call C_NAME(chkr_perror)
	call C_NAME(chkr_abort)
here:	jmp here		# /* infinite loop */
	SIZE(abort)

# /* make the syscall */
	FUNCTION(make_syscall)
	.globl C_NAME(make_syscall)
C_NAME(make_syscall):		# /* just do the int instruction */
	pusha			# /* save register for gcc */
#ifndef CHKR_IS_SAFE
	movl Reip, %esi
	cmpw $0x80cd, (%esi)	# /* check for int $0x80 */
	jne abort		# /* check fails */
#endif /* CHKR_IS_SAFE */
	movb Rflag, %ah		# /* restore flags */
	sahf
	movl Reax, %eax		# /* restore registers */
	movl Rebx, %ebx
	movl Recx, %ecx
	movl Redx, %edx
	movl Resi, %esi
	movl Redi, %edi
	int $0x80		# /* make the sys call */
	movl %eax, Reax		# /* save registers */
	movl %ebx, Rebx
	movl %ecx, Recx
	movl %edx, Redx
	movl %esi, Resi
	movl %edi, Redi
	lahf			# /* save flag */
	movb %ah, Rflag
	movl Resp, %esi
#ifndef CHKR_IS_SAFE
	movl Reip, %ecx		# /* Be sure about the eip */
	cmpl -4(%esi), %ecx
	jne abort		# /* check fails */
#endif	/* !CHKR_IS_SAFE */
	addl $2,Reip		# /* forget the int instruction */
	addl $2,-4(%esi)	# /* really forget */
	popa			# /* restore for gcc */
	ret
	SIZE(make_syscall)

# /* skip the syscall */
	FUNCTION(skip_syscall)
	.globl C_NAME(skip_syscall)
C_NAME(skip_syscall):		# /* just do the int instruction */
	pusha			# /* save register for gcc (secure) */
#ifndef CHKR_IS_SAFE
	movl Reip, %esi
	cmpw $0x80cd, (%esi)	# /* check for int $0x80 */
	jne abort		# /* check fails */
#endif /* CHKR_IS_SAFE */
	movl Resp, %esi
#ifndef CHKR_IS_SAFE
	movl Reip, %ecx		# /* Be sure about the eip */
	cmpl -4(%esi), %ecx
	jne abort		# /* check fails */
#endif	/* !CHKR_IS_SAFE */
	addl $2,Reip		# /* forget the int instruction */
	addl $2,-4(%esi)	# /* really forget */
	popa			# /* restore for gcc */
	ret
	SIZE(skip_syscall)

/* chkr functions have this prototype:
        chkr_?_?_?_?_chkr
             | | | |
             | | | +------------------ size of the memory zone
             | | +-------------------- right demanded
             | +---------------------- addressing mode 1:disp 2:Modrm 3:str
             |                           4:int 5:stack 6:Modrm 7:call 8:Jmp
             +------------------------ bytes to skip
   str mode is special: accmode: 1 is repne, 2 is repe/rep, 0 means no prefix
*/


#define chkr_1_2_x_x_chkr(accm,s) \
	.globl chkr_1_2_##accm##_##s##_chkr; \
	FUNCTION(chkr_1_2_##accm##_##s##_chkr); \
chkr_1_2_##accm##_##s##_chkr:; \
	call save_register; \
	pushl %ebp; \
	movl %esp, %ebp; \
	movl $##s##,size; \
	movl $##accm##,accmod; \
	incl %esi; \
	call Chkr_Modrm; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_1_2_##accm##_##s##_chkr)

#define chkr_x_2_x_x_chkr(l,accm,s) \
	.globl chkr_##l##_2_##accm##_##s##_chkr; \
	FUNCTION(chkr_##l##_2_##accm##_##s##_chkr); \
chkr_##l##_2_##accm##_##s##_chkr:; \
	call save_register; \
	pushl %ebp; \
	movl %esp, %ebp; \
	movl $##s##,size; \
	movl $##accm##,accmod; \
	addl $##l##, %esi; \
	call Chkr_Modrm; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_##l##_2_##accm##_##s##_chkr)

#define chkr_1_1_x_x_chkr(accm,s) \
	.globl chkr_1_1_##accm##_##s##_chkr; \
	FUNCTION(chkr_1_1_##accm##_##s##_chkr); \
chkr_1_1_##accm##_##s##_chkr:; \
	call save_register; \
	pushl %ebp; \
	movl %esp, %ebp; \
	movl $##s##,size; \
	movl $##accm##,accmod; \
	movl 1(%esi), %edi; \
	call Check_addr; \
	pop %ebp; \
	call restore_register; \
	ret: \
	SIZE(chkr_1_1_##accm##_##s##_chkr)

#define chkr_x_1_x_x_chkr(l,accm,s) \
	.globl chkr_##l##_1_##accm##_##s##_chkr; \
	FUNCTION(chkr_##l##_1_##accm##_##s##_chkr); \
chkr_##l##_1_##accm##_##s##_chkr:; \
	call save_register; \
	pushl %ebp; \
	movl %esp, %ebp; \
	movl $##s##,size; \
	movl $##accm##,accmod; \
	movl l##(%esi), %edi; \
	call Check_addr; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_##l##_1_##accm##_##s##_chkr)

/* be careful: do not change flags before calling save_register */
/* This function is called by a code such as:
 *	push $addr		* push the effective addr
 *	call chkr_0_6_x_x_chkr	* call this function
 *	movl addr, %esi		* the instruction
 */
#define chkr_0_6_x_x_chkr(accm,s) \
	.globl chkr_0_6_##accm##_##s##_chkr; \
	FUNCTION(chkr_0_6_##accm##_##s##_chkr); \
chkr_0_6_##accm##_##s##_chkr:; \
	movl %eax, addr; /* save %eax */ \
	movl (%esp), %eax; /* load the old eip */ \
	xchgl 4(%esp), %eax; /* change the addr and the old eip */ \
	xchgl %eax, addr; /* save addr and restore %eax */ \
	leal 4(%esp), %esp; /* adjust esp */ \
	call save_register;\
	push %ebp; \
	movl %esp, %ebp; \
	pushl $##accm##; \
	pushl $0x##s##; \
	pushl addr; \
	call C_NAME(chkr_check_addr); \
	addl $12, %esp; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_0_6_##accm##_##s##_chkr)

/* For fldenv, fstenv: chkr_x_1_x_b_chkr.  In fact b means 28 bytes. */
#define chkr_0_6_x_b_chkr(accm) \
	.globl chkr_0_6_##accm##_b_chkr; \
	FUNCTION(chkr_0_6_##accm##_b_chkr); \
chkr_0_6_##accm##_b_chkr:; \
	movl %eax, addr; /* save %eax */ \
	movl (%esp), %eax; /* load the old eip */ \
	xchgl 4(%esp), %eax; /* change the addr and the old eip */ \
	xchgl %eax, addr; /* save addr and restore %eax */ \
	leal 4(%esp), %esp; /* adjust esp */ \
	call save_register; \
	push %ebp; \
	movl %esp, %ebp; \
	pushl $##accm##; \
	pushl $0x1c; \
	pushl addr; \
	call C_NAME(chkr_check_addr); \
	addl $12, %esp; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_0_6_##accm##_b_chkr)


/* be careful: do not change flags before calling save_register */
/* This function is called by a code such as:
 *	push %eax		* save %eax
 *	leal 4(%ebx,2), %eax	* put the effective addr in %eax
 *	call chkr_1_6_x_x_chkr	* call this function
 *	movl 4(%ebx,2), %esi	* the instruction
 */
#define chkr_1_6_x_x_chkr(accm,s) \
	.globl chkr_1_6_##accm##_##s##_chkr; \
	FUNCTION(chkr_1_6_##accm##_##s##_chkr); \
chkr_1_6_##accm##_##s##_chkr:; \
	movl %eax, addr; /* save the addr */ \
	movl (%esp), %eax; /* load the old eip */ \
	xchgl 4(%esp), %eax; /* save the old eip */ \
	leal 4(%esp), %esp; /* change %esp */ \
	call save_register; \
	push %ebp; \
	movl %esp, %ebp; \
	pushl $##accm##; \
	pushl $0x##s##; \
	pushl addr; \
	call C_NAME(chkr_check_addr); \
	addl $12, %esp; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_1_6_##accm##_##s##_chkr)

/* For fldenv, fstenv: chkr_x_1_x_b_chkr.  In fact b means 28 bytes. */
#define chkr_1_6_x_b_chkr(accm) \
	.globl chkr_1_6_##accm##_b_chkr; \
	FUNCTION(chkr_1_6_##accm##_b_chkr); \
chkr_1_6_##accm##_b_chkr:; \
	movl %eax, addr; /* save the addr */ \
	movl (%esp), %eax; /* load the old eip */ \
	xchgl 4(%esp), %eax; /* save the old eip */ \
	leal 4(%esp), %esp; /* change %esp */ \
	call save_register; \
	push %ebp; \
	movl %esp, %ebp; \
	pushl $##accm##; \
	pushl $0x1c; \
	pushl addr; \
	call C_NAME(chkr_check_addr); \
	addl $12, %esp; \
	pop %ebp; \
	call restore_register; \
	ret; \
	SIZE(chkr_1_6_##accm##_b_chkr)

#ifdef OLD_CHKR
chkr_1_2_x_x_chkr(1,1);		# /* size = 1, accmod = read */
chkr_1_2_x_x_chkr(2,1);		# /* size = 1, accmod = write */
chkr_1_2_x_x_chkr(3,1);		# /* size = 1, accmod = read & write */

chkr_1_2_x_x_chkr(1,2);		# /* size = 2, accmod = read */
chkr_1_2_x_x_chkr(2,2);		# /* size = 2, accmod = write */

chkr_1_2_x_x_chkr(1,4);		# /* size = 4, accmod = read */
chkr_1_2_x_x_chkr(2,4);		# /* size = 4, accmod = write */
chkr_1_2_x_x_chkr(3,4);		# /* size = 4, accmod = read & write */

chkr_1_2_x_x_chkr(1,8);		# /* size = 8, accmod = read */
chkr_1_2_x_x_chkr(2,8);		# /* size = 8, accmod = write */

chkr_x_2_x_x_chkr(2,1,1);	# /* size = 1, accmod = read */

chkr_x_2_x_x_chkr(2,1,2);	# /* size = 2, accmod = read */
chkr_x_2_x_x_chkr(2,2,2);	# /* size = 2, accmod = write */
chkr_x_2_x_x_chkr(2,3,2);	# /* size = 2, accmod = read & write */

chkr_x_2_x_x_chkr(2,1,4);	# /* size = 4, accmod = read */
chkr_x_2_x_x_chkr(2,3,4);	# /* size = 4, accmod = read & write */

chkr_x_2_x_x_chkr(3,1,1);	# /* size = 1, accmod = read */
chkr_x_2_x_x_chkr(3,1,2);	# /* size = 2, accmod = read */
chkr_x_2_x_x_chkr(3,1,4);	# /* size = 4, accmod = read */

chkr_1_1_x_x_chkr(1,4);		# /* size = 4, accmod = read */
chkr_1_1_x_x_chkr(2,4);		# /* size = 4, accmod = write */

chkr_x_1_x_x_chkr(2,1,4);	# /* size = 4, accmod = read */
chkr_x_1_x_x_chkr(2,2,4);	# /* size = 4, accmod = write */
#endif /* OLD_CHKR */

chkr_0_6_x_x_chkr(1,1);		# /* size = 1, accmod = read */
chkr_0_6_x_x_chkr(2,1);		# /* size = 1, accmod = write */
chkr_0_6_x_x_chkr(3,1);		# /* size = 1, accmod = read/write */

chkr_0_6_x_x_chkr(1,2);		# /* size = 2, accmod = read */
chkr_0_6_x_x_chkr(2,2);		# /* size = 2, accmod = write */
chkr_0_6_x_x_chkr(3,2);		# /* size = 2, accmod = read/write */

chkr_0_6_x_x_chkr(1,4);		# /* size = 4, accmod = read */
chkr_0_6_x_x_chkr(2,4);		# /* size = 4, accmod = write */
chkr_0_6_x_x_chkr(3,4);		# /* size = 4, accmod = read/write */

chkr_0_6_x_x_chkr(1,8);		# /* size = 8, accmod = read */
chkr_0_6_x_x_chkr(2,8);		# /* size = 8, accmod = write */
chkr_0_6_x_x_chkr(3,8);		# /* size = 8, accmod = read/write */

chkr_0_6_x_x_chkr(1,a);		# /* size = 10, accmod = read */
chkr_0_6_x_x_chkr(2,a);		# /* size = 10, accmod = write */

chkr_1_6_x_x_chkr(1,1);		# /* size = 1, accmod = read */
chkr_1_6_x_x_chkr(2,1);		# /* size = 1, accmod = write */
chkr_1_6_x_x_chkr(3,1);		# /* size = 1, accmod = read/write */

chkr_1_6_x_x_chkr(1,2);		# /* size = 2, accmod = read */
chkr_1_6_x_x_chkr(2,2);		# /* size = 2, accmod = write */
chkr_1_6_x_x_chkr(3,2);		# /* size = 2, accmod = read/write */

chkr_1_6_x_x_chkr(1,4);		# /* size = 4, accmod = read */
chkr_1_6_x_x_chkr(2,4);		# /* size = 4, accmod = write */
chkr_1_6_x_x_chkr(3,4);		# /* size = 4, accmod = read/write */

chkr_1_6_x_x_chkr(1,8);		# /* size = 8, accmod = read */
chkr_1_6_x_x_chkr(2,8);		# /* size = 8, accmod = write */
chkr_1_6_x_x_chkr(3,8);		# /* size = 8, accmod = read/write */

chkr_1_6_x_x_chkr(1,a);		# /* size = 10, accmod = read */
chkr_1_6_x_x_chkr(2,a);		# /* size = 10, accmod = write */

chkr_1_6_x_b_chkr(1);		# /* size = 28, accmod = read */
chkr_1_6_x_b_chkr(2);		# /* size = 28, accmod = write */
chkr_0_6_x_b_chkr(1);		# /* size = 28, accmod = read */
chkr_0_6_x_b_chkr(2);		# /* size = 28, accmod = write */

/* entry point for repne xxx instructions */
	FUNCTION(chkr_1_3_1_0_chkr)
	.globl chkr_1_3_1_0_chkr
chkr_1_3_1_0_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	testl %ecx, %ecx
	je L20
	testl $0x00000400, Rflag
	jne abort
	# /* cld */
Lrepnecld:
	cmpb $0xae, 1(%esi)	# /* is repne scasb */
	je Lzscasb
	jmp abort		# /* don't know */
Lzscasb:
	movl Reax, %eax		# /* load aex */
	repne			# /* execute the instruction */
	scasb
	subl Recx, %ecx		# /* compute the lenth */
	negl %ecx		# /* since we need Recx - %ecx */
	movl %ecx, size
	movl $1, accmod		# /* access is read */
	movl Redi, %edi
	call Check_addr		# /* go... */
L20:	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_3_1_0_chkr)

/* entry point for rep/repe/repz xxx instructions */
	FUNCTION(chkr_1_3_2_0_chkr)
	.globl chkr_1_3_2_0_chkr
chkr_1_3_2_0_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	testl %ecx,%ecx		# /* nothing to check */
	je L30
	testl $0x00000400, Rflag
	je Lrepecld
	cmpb $0xa4, 1(%esi)
	je Lsemovsb		# /* is rep movsb */
	cmpb $0xa5, 1(%esi)
	je Lsemovsl		# /* is rep movsl */
	jmp abort
Lrepecld:			# /* cld */
	cmpb $0xa5, 1(%esi)	# /* is rep movsl */
	je Lcemovsl
	cmpb $0xa6, 1(%esi)	# /* is repz cmpsb */
	je Lcecmpsb
	cmpb $0xaa, 1(%esi)	# /* is rep stosb */
	je Lcestosb
	cmpb $0xab, 1(%esi)	# /* is rep stosl */
	je Lcestosl
	cmpb $0xa4, 1(%esi)	# /* is rep movsb */
	je Lcemovsb
	jmp abort		# /* don't know */
Lcemovsl:			# /* cld rep movsl */
	movl $2, accmod		# /* destination: write */
	shll $2, %ecx		# /* convert nbr of dword to nbr of bytes */
	movl %ecx, size		# /* load the lenth */
	call Check_addr		# /* first check */
	movl Resi, %edi		# /* source is in Resi */
	movl $1, accmod		# /* one read the source */
	movl Recx, %ecx
	shll $2, %ecx
	movl %ecx, size
	call Check_addr
L30:	pop %ebp
	call restore_register
	ret
Lsemovsl:			# /* std rep movsl */
	movl $2, accmod
	shll $2, %ecx
	movl %ecx, size
	subl %ecx, %edi
	addl $4, %edi
	call Check_addr
	movl Resi, %edi
	movl $1, accmod
	movl Recx, %ecx
	shll $2, %ecx
	subl %ecx, %edi
	incl %edi
	movl %ecx, size
	call Check_addr
	pop %ebp
	call restore_register
	ret
Lcemovsb:			# /* cld rep movsb */
	movl $2, accmod		# /* destination: write */
	movl %ecx, size		# /* load the lenth */
	call Check_addr		# /* first check */
	movl Resi, %edi		# /* source is in Resi */
	movl $1, accmod		# /* one read the source */
	movl Recx, %ecx
	movl %ecx, size
	call Check_addr
	pop %ebp
	call restore_register
	ret
Lsemovsb:			# /* std rep movsb */
	movl $2, accmod
	movl %ecx, size
	subl %ecx, %edi
	incl %edi
	call Check_addr
	movl $1, accmod
	movl Resi, %edi
	movl Recx, %ecx
	subl %ecx, %edi
	incl %edi
	movl %ecx, size
	call Check_addr
	pop %ebp
	call restore_register
	ret
Lcestosb:			# /* cld rep stosb */
	movl $2, accmod		# /* destination: write */
	movl %ecx, size		# /* load the lenth */
	call Check_addr		# /* first check */
	pop %ebp
	call restore_register
	ret
Lcestosl:
	movl $2, accmod		# /* destination: write */
	shll $2, %ecx		# /* covert nbr of dword to nbr of bytes */
	movl %ecx, size		# /* load the lenth */
	call Check_addr		# /* first check */
	pop %ebp
	call restore_register
	ret
Lcecmpsb:
	movl Resi, %esi		# /* secure */
	movl Recx, %ecx
	movl Redi, %edi
	repz			# /* execute the instruction */
	cmpsb
	subl Recx, %ecx		# /* Now, we known the size */
	negl %ecx		# /* In fact, we want Recx - %ecx */
	movl %ecx, size
	pushl $1		# /* accmod is read */
	pushl %ecx
	pushl Redi		# /* first operand */
	call C_NAME(chkr_check_addr)
	addl $12, %esp
	pushl $1
	pushl size
	pushl Resi		# /* second operand */
	call C_NAME(chkr_check_addr)
	addl $12, %esp
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_3_2_0_chkr)

/* entry point for lodsb, scasb, stosb instructions */
	FUNCTION(chkr_1_3_0_1_chkr)
	.globl chkr_1_3_0_1_chkr
chkr_1_3_0_1_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	cmpb $0xac, (%esi)	# /* is lodsb */
	je Llodsb
	cmpb $0xae, (%esi)	# /* is scasb */
	je Lscasb
	cmpb $0xaa, (%esi)	# /* is stosb */
	je Lstosb
	cmpb $0xa4, (%esi)	# /* is movsb */
	je Lmovsb
	jmp abort		# /* don't know */
Llodsb:	movl $1, accmod		# /* source: read */
	movl $1, size		# /* load the lenth 1:char size */
	movl Resi, %edi
	call Check_addr		# /* first check */
	pop %ebp
	call restore_register
	ret
Lscasb:	movl $1, accmod		# /* source: read */
	movl $1, size		# /* load the lenth 1:char size */
	call Check_addr		# /* first check */
	pop %ebp
	call restore_register
	ret
Lstosb: movl $2, accmod		# /* source: write */
	movl $1, size		# /* byte */
	call Check_addr
	pop %ebp
	call restore_register
	ret
Lmovsb:	movl $2, accmod
	movl $1, size
	call Check_addr
	movl $1, accmod
	movl Resi, %edi
	call Check_addr
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_3_0_1_chkr)

/* entry point for movsw instructions */
	FUNCTION(chkr_2_3_0_2_chkr)
	.globl chkr_2_3_0_2_chkr
chkr_2_3_0_2_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	cmpb $0xa5, 1(%esi)	# /* is movsw */
	je Lmovsw
	jmp abort		# /* don't know */
Lmovsw:	movl $1, accmod		# /* source: read */
	movl $2, size		# /* load the lenth 2:word size */
	movl Resi, %edi
	call Check_addr		# /* first check */
	movl $2, accmod		# /* destination: write */
	movl Redi, %edi
	call Check_addr
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_2_3_0_2_chkr)

/* entry point for int $0x80 (ie syscall entry). */
/* A frame pointer is made */
	FUNCTION(chkr_1_4_8_0_chkr)
	.globl chkr_1_4_8_0_chkr
chkr_1_4_8_0_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	movl Reip, %eax		# /* return address */
	pushl %eax
	pushl %ebp		# /* push the frame pointer */
	movl %esp, %ebp
	pushl $C_NAME(linux_context)
	call C_NAME(check_syscall)	# /* really check */
	addl $12,%esp
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_4_8_0_chkr)

/* entry point for StOp. It is designed to be quick */
	FUNCTION(chkr_0_5_0_0_chkr)
	.globl chkr_0_5_0_0_chkr
chkr_0_5_0_0_chkr:
	pushl %eax
	pushf
	movl %esp, %eax
	addl $0x0c, %eax	# /* %eax, %eip and flags*/
	movl %eax, C_NAME(known_stack_limit)
	cmpl %eax, C_NAME(stack_bitmapped)
	jge L40
	pusha
        call C_NAME(adjust_bm_when_stack_reduce)
	popa
L40:	popf
	popl %eax
	ret
	SIZE(chkr_0_5_0_0_chkr)

/* entry point for "ret". It is designed to be quick */
	FUNCTION(chkr_1_5_0_4_chkr)
	.globl chkr_1_5_0_4_chkr
chkr_1_5_0_4_chkr:
	pushl %eax
	pushf
	movl %esp, %eax
	addl $0x10, %eax	# /* %eflags, %eax and %eip^2 */
	movl %eax, C_NAME(known_stack_limit)
	cmpl %eax, C_NAME(stack_bitmapped)
	jge L41
	pusha
	call C_NAME(adjust_bm_when_stack_reduce)
	popa
L41:	popf
	popl %eax
	ret
	SIZE(chkr_1_5_0_4_chkr)

/* entry point for "ret x". It is designed to be quick */
	FUNCTION(chkr_1_5_0_0_chkr)
	.globl chkr_1_5_0_0_chkr
chkr_1_5_0_0_chkr:
	pushl %eax
	pushl %esi
	pushf
	movl %esp, %esi
	movl 12(%esi), %esi	# /* %esi equals old %eip */
	movzwl 1(%esi), %eax	# /* %eax contains the disp */
	addl $0x14, %eax	# /* %eflags, %eax, %esi, and %eip^2 */
	addl %esp, %eax
	movl %eax, C_NAME(known_stack_limit)
	cmpl %eax, C_NAME(stack_bitmapped)
	jge L42
	pusha
	call C_NAME(adjust_bm_when_stack_reduce)
	popa
L42:	popf
	popl %esi
	popl %eax
	ret
	SIZE(chkr_1_5_0_0_chkr)

/* entry point for pushl */
	FUNCTION(chkr_0_5_2_4_chkr)
	.globl chkr_0_5_2_4_chkr
chkr_0_5_2_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x03	# /* right is read and write */
	pushl $0x04	# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax	# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp	# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_0_5_2_4_chkr)

/* entry point for pushl %ebp */
	FUNCTION(chkr_0_5_0_4_chkr)
	.globl chkr_0_5_0_4_chkr
chkr_0_5_0_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x00		# /* right is none */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_0_5_0_4_chkr)

/* entry point for popl */
	FUNCTION(chkr_0_5_1_4_chkr)
	.globl chkr_0_5_1_4_chkr
chkr_0_5_1_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $0x01		# /* right is read */
	pushl $0x04		# /* length is 4 (long) */
	pushl Resp		# /* addr is %esp */
	call C_NAME(chkr_check_addr)
	addl $12, %esp		# /* normalize the stack */
	addl $0x04, C_NAME(known_stack_limit)
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_0_5_1_4_chkr)

/* entry point for popa */
	FUNCTION(chkr_1_5_1_1_chkr)
	.globl chkr_1_5_1_1_chkr
chkr_1_5_1_1_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $0x01		# /* right is read */
	pushl $0x20		# /* length is 8*4 (long) */
	pushl Resp		# /* addr is %esp */
	call C_NAME(chkr_check_addr)
	addl $12, %esp		# /* normalize the stack */
	addl $0x04, C_NAME(known_stack_limit)
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_5_1_1_chkr)

/* entry point for pushl (addr)*/
	FUNCTION(chkr_2_6_2_4_chkr)
	.globl chkr_2_6_2_4_chkr
chkr_2_6_2_4_chkr:
	movl %eax, addr 	/* save addr */
	movl (%esp), %eax	/* load the old eip */
	xchgl 4(%esp), %eax	/* change the addr and the old eip */
	leal 4(%esp), %esp	/* adjust esp */
	call save_register
	pushl %ebp
	movl %esp, %ebp
	pushl $1
	pushl $4
	pushl addr
	call C_NAME(chkr_check_addr)
	addl $12, %esp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x03		# /* right is read and write */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_2_6_2_4_chkr)

/* entry point for pushl (addr)*/
	FUNCTION(chkr_1_5_2_4_chkr)
	.globl chkr_1_5_2_4_chkr
chkr_1_5_2_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $1
	pushl $4
	pushl 1(%esi)
	call C_NAME(chkr_check_addr)	# /* check addr can be read */
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x03		# /* right is read and write */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_5_2_4_chkr)

/* entry point for popl (addr)*/
	FUNCTION(chkr_1_5_1_4_chkr)
	.globl chkr_1_5_1_4_chkr
chkr_1_5_1_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $2
	pushl $4
	pushl 1(%esi)
	call C_NAME(chkr_check_addr)	# /* check addr can be written */
	addl $12, %esp
	pushl $0x01		# /* right is read */
	pushl $0x04		# /* length is 4 (long) */
	pushl Resp		# /* addr is %esp */
	call C_NAME(chkr_check_addr)
	addl $12, %esp		# /* normalize the stack */
	addl $0x04, C_NAME(known_stack_limit)
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_5_1_4_chkr)

/* entry point for call addr
 * addr is an immediat. Just protects the stack.
 * Used by sigjumpto.S
 */
	FUNCTION(chkr_0_7_0_4_chkr)
	.globl chkr_0_7_0_4_chkr
chkr_0_7_0_4_chkr:
	call save_register
	pushl %ebp
        movl %esp,%ebp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x00		# /* right is none */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_0_7_0_4_chkr)

/* for call *xxx, where xxx is mod/rm
 * xxx is at first checked, and then the stack is protected: you can't read
 * the saved eip.
 */
	FUNCTION(chkr_1_7_1_4_chkr)
	.globl chkr_1_7_1_4_chkr
chkr_1_7_1_4_chkr:
	movl %eax, addr		/* save the addr */ 
	movl (%esp), %eax	/* load the old eip */ 
	xchgl 4(%esp), %eax	/* save the old eip */ 
	leal 4(%esp), %esp	/* change %esp */ 
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $1
	pushl $4
	pushl addr		/* test xxx */
	call C_NAME(chkr_check_addr)
	movl addr, %eax
	pushl (%eax)
	call C_NAME(chkr_check_exec)
	addl $16, %esp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x00		# /* right is none */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_7_1_4_chkr)

/* for call *addr.
 * The EA is given by the stack.
 * xxx is at first checked, and then the stack is protected: you can't read
 * the saved eip.
 */
	FUNCTION(chkr_0_7_1_4_chkr)
	.globl chkr_0_7_1_4_chkr
chkr_0_7_1_4_chkr:
	movl %eax, addr		/* save the %eax */ 
	movl (%esp), %eax	/* load the old eip */ 
	xchgl 4(%esp), %eax	/* save the old eip and load the EA */
	xchgl %eax, addr;       /* save addr and restore %eax */
	leal 4(%esp), %esp	/* change %esp */ 
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl addr
	call C_NAME(chkr_check_exec)
	addl $16, %esp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x00		# /* right is none */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_0_7_1_4_chkr)

/* for call *reg, the stack is protected: you can't read the saved eip.
 *  The EA (effective address) is given by the stack.
 */
	FUNCTION(chkr_3_7_0_4_chkr)
	.globl chkr_3_7_0_4_chkr
chkr_3_7_0_4_chkr:
	movl %eax, addr		/* save the %eax */ 
	movl (%esp), %eax	/* load the old eip */ 
	xchgl 4(%esp), %eax	/* save the old eip and load the EA */
	xchgl %eax, addr;       /* save addr and restore %eax */
	leal 4(%esp), %esp	/* change %esp */ 
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl addr
	call C_NAME(chkr_check_exec)
	addl $4, %esp
	subl $0x04, C_NAME(known_stack_limit)
	pushl $0x00		# /* right is none */
	pushl $0x04		# /* length is 4 (long) */
	movl Resp, %eax
	subl $0x04, %eax
	pushl %eax		# /* addr is %esp - 4*/
	call C_NAME(chkr_set_right)
	addl $12, %esp		# /* normalize the stack */
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_3_7_0_4_chkr)

/* For jmp *addr.
 * The EA is given by %eax.  Check ADDR is readable and *ADDR is executable.
 */
	FUNCTION(chkr_1_8_1_4_chkr)
	.globl chkr_1_8_1_4_chkr
chkr_1_8_1_4_chkr:
	movl %eax, addr	 		/* save addr */
	movl (%esp), %eax 		/* load the old eip */
	xchgl 4(%esp), %eax 		/* save the old eip and load %eax */
	leal 4(%esp), %esp 		/* change %esp */
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl $1
	pushl $0x4
	pushl addr
	call C_NAME(chkr_check_addr)
	movl addr, %eax
	pushl (%eax)
	call C_NAME(chkr_check_exec)
	addl $16, %esp
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_1_8_1_4_chkr)

/* For jmp *reg */
/* The EA is given by the stack */
	FUNCTION(chkr_3_8_0_4_chkr)
	.globl chkr_3_8_0_4_chkr
chkr_3_8_0_4_chkr:
	movl %eax, addr		/* save %eax */
	movl (%esp), %eax 	/* load the old eip */
	xchgl 4(%esp), %eax 	/* save the old eip and load the EA */
	xchgl %eax, addr;       /* save EA and restore %eax */
	leal 4(%esp), %esp 	/* change %esp */
	call save_register
	pushl %ebp
        movl %esp,%ebp
	pushl addr
	call C_NAME(chkr_check_exec)
	addl $4, %esp
	pop %ebp
	call restore_register
	ret
	SIZE(chkr_3_8_0_4_chkr)
