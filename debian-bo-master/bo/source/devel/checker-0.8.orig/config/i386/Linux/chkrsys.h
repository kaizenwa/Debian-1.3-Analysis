/* Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <sys/syscall.h>

/* Not that using a `PASTE' macro loses.  */
#ifdef	__STDC__

#ifdef __ELF__
#define	SYSCALL(name,args)	PSEUDO (name, name, args) \
.type chkr_##name,@function; \
.L_chkr_##name##end: .size chkr_##name,.L_chkr_##name##end - chkr_##name

#else /* __ELF__ */

/* Regular a.out definition */
#define	SYSCALL(name,args)	PSEUDO (name, name, args)

#endif /* __ELF__ */

#else  /* __STDC__ */

#define	SYSCALL__(name,args)	PSEUDO (__/**/name, name, args)
#define	SYSCALL(name,args)	PSEUDO (name, name, args)

#endif /* __STDC__ */

/* Machine-dependent sysdep.h files are expected to define the macro
   PSEUDO (function_name, syscall_name) to emit assembly code to define the
   C-callable function FUNCTION_NAME to do system call SYSCALL_NAME.
   r0 and r1 are the system call outputs.  movl should be defined as
   an instruction such that "movl r1, r0" works.  ret should be defined
   as the return instruction.  */


#if !defined(HAVE_GNU_LD) && !defined (__ELF__)
#define	 ___errno	_errno
#endif

#define	HAVE_SYSCALLS

#if defined(__i486__) || defined(i486)
#define ALIGN 4
#else
#define ALIGN 2
#endif

#ifdef __ELF__
#define SYMBOL_NAME(X) chkr_##X
#define SYMBOL_NAME_LABEL(X) chkr_##X##:
#else
#define SYMBOL_NAME(X) _chkr_##X
#define SYMBOL_NAME_LABEL(X) _chkr_##X##:
#endif

#define ERRNO SYMBOL_NAME(errno)

#ifdef __ELF__
#define	ENTRY(name)							      \
  .globl SYMBOL_NAME(name);						      \
  .globl ERRNO;								      \
  .align ALIGN;								      \
  SYMBOL_NAME_LABEL(name)
#else
#define	ENTRY(name)							      \
  .globl SYMBOL_NAME(name);						      \
  .align ALIGN;								      \
  SYMBOL_NAME_LABEL(name)
#endif

/* Use this for the different syntaxes for local assembler labels */
#ifdef __ELF__
#define L(X) .L##X
#define LF(X) .L##X
#define LL(X) .L##X##:
#else
#define L(X) X
#define LF(X) X##f
#define LL(X) X##:
#endif

#if defined(__PIC__) || defined (__pic__)
#define	PSEUDO(name, syscall_name, args)				      \
  .text;								      \
  ENTRY (name)								      \
    pushl %ebp;								      \
    movl %esp,%ebp;							      \
    PUSH_##args								      \
    call L(L4);								      \
   LL(L4)								      \
    popl %ebx;								      \
    addl $_GLOBAL_OFFSET_TABLE_+[.-L(L4)],%ebx;				      \
    pushl %ebx;								      \
    movl $SYS_##syscall_name, %eax;					      \
    MOVE_##args								      \
    int $0x80;								      \
    popl %ebx;								      \
    movl %eax,%edx;							      \
    test %edx,%edx;							      \
    jge	L(Lexit);							      \
    negl %edx;								      \
    movl ERRNO@GOT(%ebx),%eax;						      \
    movl %edx,(%eax);							      \
    movl $-1,%eax;							      \
   LL(Lexit)								      \
    POP_##args 								      \
    movl %ebp,%esp;							      \
    popl %ebp;

#else

#define	PSEUDO(name, syscall_name, args)				      \
  .text;								      \
  ENTRY (name)								      \
    pushl %ebp;								      \
    movl %esp,%ebp;							      \
    PUSH_##args								      \
    movl $(SYS_##syscall_name), %eax;					      \
    MOVE_##args								      \
    int $0x80;								      \
    test %eax, %eax;							      \
    jge	L(Lexit);							      \
    negl %eax;								      \
    movl %eax,ERRNO;							      \
    movl $-1,%eax;							      \
   LL(Lexit)								      \
    POP_##args								      \
    movl %ebp,%esp;							      \
    popl %ebp;
 
#endif

/* Linux takes system call arguments in registers:
	0: %eax	This is the system call number.
   	1: %ebx This is the first argument.
	2: %ecx
	3: %edx
	4: %esi
	5: %edi
 */

#if defined(__PIC__) || defined (__pic__)
#define PUSH_0	pushl %ebx;
#else
#define PUSH_0	/* No arguments to push.  */
#endif
#define PUSH_1	pushl %ebx;
#define PUSH_2	PUSH_1
#define PUSH_3	PUSH_1
#define PUSH_4	pushl %esi; PUSH_3
#define PUSH_5	pushl %edi; PUSH_4

#define	MOVE_0	/* No arguments to move.  */
#define	MOVE_1	movl 8(%ebp),%ebx;
#define	MOVE_2	MOVE_1 movl 12(%ebp),%ecx;
#define	MOVE_3	MOVE_2 movl 16(%ebp),%edx;
#define	MOVE_4	MOVE_3 movl 20(%ebp),%esi;
#define	MOVE_5	MOVE_4 movl 24(%ebp),%edi;

#if defined(__PIC__) || defined (__pic__)
#define POP_0	popl %ebx;
#else
#define POP_0	/* No arguments to pop.  */
#endif
#define POP_1	popl %ebx;
#define POP_2	POP_1
#define POP_3	POP_1
#define POP_4	POP_3 popl %esi;
#define POP_5	POP_4 popl %edi;

/* Linux doesn't use it. */
#if 0
#define	r0	%eax
#define	r1	%edx
#define MOVE(x,y)	movl x , y
#endif
