/* syscalls.
   Copyright 1995 Tristan Gingold
		  Written June 1995 by Tristan Gingold

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
	.section ".text"
	.align 4
.Ltext0:

#ifndef __PIC__
#define DO_SYSCALL_WITH_RESTART(name, nbr)	\
name:						;\
	.type  name,@function			;\
	.globl name				;\
	mov  nbr, %g1				;\
1:	ta  8					;\
	bcc  2f					;\
	cmp %o0, 91				;\
	be 1b					;\
	sethi  %hi(chkr_errno), %o5		;\
	st %o0, [%o5 + %lo(chkr_errno)]		;\
	retl					;\
	mov  -1, %o0				;\
2:	retl					;\
	nop;

#define DO_SYSCALL_WITH_ERRNO(name, nbr)	\
name:						;\
	.type  name,@function			;\
	.globl name				;\
	mov  nbr, %g1				;\
	ta  8					;\
	bcc  2f					;\
	sethi  %hi(chkr_errno), %o5		;\
	st %o0, [%o5 + %lo(chkr_errno)]		;\
	retl					;\
	mov  -1, %o0				;\
2:	retl					;\
	nop;

/* Be careful: it only works with 3 arguments (or less).
 * FIXME: Add other mov ?  */
#define DO_SYSCALL2_WITH_ERRNO(name, nbr, nbr1)	\
name:						;\
	.type  name,@function			;\
	.globl name				;\
	mov %o2, %o3				;\
	mov %o1, %o2				;\
	mov %o0, %o1				;\
	mov nbr1, %o0				;\
	mov  nbr, %g1				;\
	ta  8					;\
	bcc  2f					;\
	sethi  %hi(chkr_errno), %o5		;\
	st %o0, [%o5 + %lo(chkr_errno)]		;\
	retl					;\
	mov  -1, %o0				;\
2:	retl					;\
	nop;

#else

#define DO_SYSCALL_WITH_RESTART(name, nbr)		\
name:							;\
	.type  name,@function				;\
	.globl name					;\
	st %o0, [%sp + 68]				;\
1:	mov  nbr, %g1					;\
	ta  8						;\
	bgeu  2f					;\
	cmp %o0, 91					;\
	be,a 1b						;\
	ld [%sp + 68], %o0				;\
	mov %o7, %g1					;\
3:	call 4f						;\
	.empty						;\
4:	sethi %hi(_GLOBAL_OFFSET_TABLE_-(3b-.)), %o5	;\
	or %o5 ,%lo(_GLOBAL_OFFSET_TABLE_-(3b-.)), %o5	;\
	add %o5, %o7, %o5				;\
	mov %g1, %o7					;\
	sethi  %hi(chkr_errno), %o4			;\
	or %o4, %lo(chkr_errno), %o4			;\
	ld [%o5 + %o4], %g1				;\
	st %o0, [%g1]					;\
	retl						;\
	mov -1, %o0
2:	retl						;\
	nop

#define DO_SYSCALL_WITH_ERRNO(name, nbr)		\
name:							;\
	.type  name,@function				;\
	.globl name					;\
	mov  nbr, %g1					;\
	ta  8						;\
	bgeu 2f						;\
	mov %o7, %g1					;\
3:	call 4f						;\
	.empty						;\
4:	sethi %hi(_GLOBAL_OFFSET_TABLE_-(3b-.)), %o5	;\
	or %o5, %lo(_GLOBAL_OFFSET_TABLE_-(3b-.)), %o5	;\
	add %o5, %o7, %o5				;\
	mov %g1, %o7					;\
	sethi  %hi(chkr_errno), %o4			;\
	or %o4, %lo(chkr_errno), %o4			;\
	ld [%o5 + %o4], %g1				;\
	st %o0, [%g1]					;\
	retl						;\
	mov -1, %o0					;\
2:	retl						;\
	nop;

/* Be careful: it only works with 3 arguments (or less).
 * FIXME: Add other mov ?  */
#define DO_SYSCALL2_WITH_ERRNO(name, nbr, nbr1)		\
name:							;\
	.type  name,@function				;\
	.globl name					;\
	save %sp, -112, %sp				;\
	mov %i2, %o3					;\
	mov %i1, %o2					;\
	mov %i0, %o1					;\
	mov nbr1, %o0					;\
	mov  nbr, %g1					;\
	ta  8						;\
	bcc  2f						;\
	nop						;\
3:	call 4f						;\
	.empty						;\
4:	sethi %hi(_GLOBAL_OFFSET_TABLE_-(3b-.)), %l7	;\
	or %l7, %lo(_GLOBAL_OFFSET_TABLE_-(3b-.)), %l7	;\
	add %l7, %o7, %l7				;\
	sethi  %hi(chkr_errno), %o5			;\
	or %o5, %lo(chkr_errno), %o5			;\
	ld [%o5 + %l7], %o1				;\
	st %o0, [%o1]					;\
	mov -1, %o0					;\
2:	ret						;\
	restore;

#endif

#define DO_SYSCALL(name, nbr)			\
name:						;\
	.type  name,@function			;\
	.globl name				;\
	mov  nbr, %g1				;\
	ta  8					;\
	retl					;\
	nop;


DO_SYSCALL(chkr__exit, 1)
DO_SYSCALL_WITH_ERRNO(chkr_read, 3)
DO_SYSCALL_WITH_ERRNO(chkr_write, 4)
DO_SYSCALL_WITH_ERRNO(chkr_open, 5)
DO_SYSCALL_WITH_ERRNO(chkr_close, 6)
DO_SYSCALL_WITH_ERRNO(chkr_link, 9)
DO_SYSCALL_WITH_ERRNO(chkr_unlink, 10)
DO_SYSCALL_WITH_ERRNO(chkr_stat, 18)
DO_SYSCALL_WITH_ERRNO(chkr_lseek, 19)
DO_SYSCALL(chkr_getpid, 20)
DO_SYSCALL_WITH_ERRNO(chkr_access, 33)
DO_SYSCALL_WITH_ERRNO(chkr_kill, 37)
DO_SYSCALL2_WITH_ERRNO(chkr_shmctl, 52, 1)
DO_SYSCALL_WITH_RESTART(chkr_ioctl, 54)
DO_SYSCALL_WITH_RESTART(chkr_fcntl, 62)
DO_SYSCALL_WITH_ERRNO(chkr_poll, 87)
DO_SYSCALL_WITH_ERRNO(chkr_sigprocmask, 95)
DO_SYSCALL_WITH_ERRNO(chkr_sigaltstack, 97)
DO_SYSCALL_WITH_ERRNO(chkr___sigaction, 98)
DO_SYSCALL2_WITH_ERRNO(chkr___getcontext, 100, 0)
DO_SYSCALL2_WITH_ERRNO(chkr__setcontext, 100, 1)
DO_SYSCALL_WITH_ERRNO(chkr_mprotect, 116)
DO_SYSCALL_WITH_ERRNO(chkr_munmap, 117)
DO_SYSCALL_WITH_ERRNO(chkr_sigpending, 99)
DO_SYSCALL_WITH_ERRNO(chkr_getdents, 81)
DO_SYSCALL_WITH_ERRNO(chkr_lstat, 88)
DO_SYSCALL_WITH_ERRNO(chkr_fstat, 28)
DO_SYSCALL_WITH_ERRNO(chkr_sysconfig, 137)
