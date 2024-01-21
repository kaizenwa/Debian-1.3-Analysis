/* Linuxthreads - a simple clone()-based implementation of Posix        */
/* threads for Linux.                                                   */
/* This file copyright (C) 1996 Richard Henderson (rth@tamu.edu)        */
/* and Xavier Leroy (Xavier.Leroy@inria.fr)                             */
/*                                                                      */
/* This program is free software; you can redistribute it and/or        */
/* modify it under the terms of the GNU General Public License          */
/* as published by the Free Software Foundation; either version 2       */
/* of the License, or (at your option) any later version.               */
/*                                                                      */
/* This program is distributed in the hope that it will be useful,      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        */
/* GNU General Public License for more details.                         */

/* Linux 2.0 system calls missing from libc 5.2.18 */

#include <asm/unistd.h>

#define SYSTEMCALL0(NAME)                                                   \
	.align 4;                                                           \
	.globl NAME;                                                        \
	.type  NAME,@function;                                              \
NAME:                                                                       \
	movl	$__NR_##NAME, %eax;                                         \
	int	$0x80;                                                      \
	testl	%eax, %eax;                                                 \
	jl	syscall_error;                                              \
	ret

#define SYSTEMCALL1(NAME)                                                   \
	.align 4;                                                           \
	.globl NAME;                                                        \
	.type  NAME,@function;                                              \
NAME:                                                                       \
        movl    %ebx, %edx;                                                 \
	movl	4(%esp), %ebx;                                              \
	movl	$__NR_##NAME, %eax;                                         \
	int	$0x80;                                                      \
        movl    %edx, %ebx;                                                 \
	testl	%eax, %eax;                                                 \
	jl	syscall_error;                                              \
	ret

#define SYSTEMCALL2(NAME)                                                   \
	.align 4;                                                           \
	.globl NAME;                                                        \
	.type  NAME,@function;                                              \
NAME:                                                                       \
        movl    %ebx, %edx;                                                 \
	movl	8(%esp), %ecx;                                              \
	movl	4(%esp), %ebx;                                              \
	movl	$__NR_##NAME, %eax;                                         \
	int	$0x80;                                                      \
        movl    %edx, %ebx;                                                 \
	testl	%eax, %eax;                                                 \
	jl	syscall_error;                                              \
	ret

	.text
SYSTEMCALL2(nanosleep)
SYSTEMCALL0(sched_yield)
SYSTEMCALL2(sched_setparam)
SYSTEMCALL2(sched_getparam)
SYSTEMCALL2(sched_setscheduler)
SYSTEMCALL1(sched_getscheduler)
SYSTEMCALL1(sched_get_priority_max)
SYSTEMCALL1(sched_get_priority_min)
SYSTEMCALL2(sched_rr_get_interval)

syscall_error:
	negl    %eax
        pushl   %eax
#ifdef __PIC__
        call    __errno_location@PLT
#else
        call    __errno_location
#endif
        popl    0(%eax)
	movl    $-1, %eax
	ret

