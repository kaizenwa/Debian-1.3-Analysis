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
	.text
	.align 4
.Ltext0:

#ifdef __PIC__
#define DO_SYSCALL_WITH_ERRNO(name, nbr)		\
	.globl _##name					;\
_##name:						;\
	mov  nbr, %g1					;\
	ta  0						;\
	bcc  4f						;\
	nop						;\
1:	call 2f						;\
	nop						;\
2:	sethi %hi(__GLOBAL_OFFSET_TABLE_-(1b-.)),%l7	;\
	or %l7,%lo(__GLOBAL_OFFSET_TABLE_-(1b-.)),%l7	;\
	add %l7,%o7,%l7					;\
	ld [%l7+_chkr_errno],%o1			;\
	st %o0,[%o1]					;\
	retl						;\
	mov  -1, %o0					;\
4:	retl						;\
	nop;

#else	/* !__PIC__ */

#define DO_SYSCALL_WITH_ERRNO(name, nbr)		\
	.globl _##name					;\
_##name:						;\
	mov  nbr, %g1					;\
	ta  0						;\
	bcc  4f						;\
	sethi  %hi(_chkr_errno), %o5			;\
	st %o0, [%o5 + %lo(_chkr_errno)]		;\
	retl						;\
	mov  -1, %o0					;\
4:	retl						;\
	nop;

#endif /* __PIC__ */

#if 0
/* Be careful: it only works with 3 arguments (or less).
 * FIXME: Add other mov ?  */
#define DO_SYSCALL2_WITH_ERRNO(name, nbr, nbr1)	\
	.globl _##name				;\
name:						;\
	mov %o2, %o3				;\
	mov %o1, %o2				;\
	mov %o0, %o1				;\
	mov nbr1, %o0				;\
	mov  nbr, %g1				;\
	ta  8					;\
	bcc  2f					;\
	sethi  %hi(_chkr_errno), %o5		;\
	st %o0, [%o5 + %lo(_chkr_errno)]	;\
	retl					;\
	mov  -1, %o0				;\
2:	retl					;\
	nop;
#endif

#define DO_SYSCALL(name, nbr)			\
	.globl _##name				;\
_##name:					;\
	mov  nbr, %g1				;\
	ta  0					;\
	retl					;\
	nop;

	.globl _chkr_mmap
_chkr_mmap:
	sethi	%hi(0x80000000), %g1
	or	%g1, %o3, %o3
	mov 	71, %g1
	ta	0
	bcc	3f
#ifdef __PIC__
	nop
1:	call 2f
	nop
2:	sethi %hi(__GLOBAL_OFFSET_TABLE_-(1b-.)),%l7
	or %l7,%lo(__GLOBAL_OFFSET_TABLE_-(1b-.)),%l7
	add %l7,%o7,%l7
	ld [%l7+_chkr_errno],%o1
	st %o0,[%o1]
#else
	sethi	%hi(_chkr_errno), %o5
	st	%o0, [%o5 + %lo(_chkr_errno)]
#endif
	retl
	mov	-1, %o0
3:	retl
	nop

DO_SYSCALL(chkr__exit, 1)
DO_SYSCALL_WITH_ERRNO(chkr_read, 3)
DO_SYSCALL_WITH_ERRNO(chkr_write, 4)
DO_SYSCALL_WITH_ERRNO(chkr_open, 5)
DO_SYSCALL_WITH_ERRNO(chkr_close, 6)
DO_SYSCALL_WITH_ERRNO(chkr_link, 9)
DO_SYSCALL_WITH_ERRNO(chkr_unlink, 10)
DO_SYSCALL_WITH_ERRNO(_chkr_brk, 17)
DO_SYSCALL_WITH_ERRNO(chkr_stat, 0x26)
DO_SYSCALL_WITH_ERRNO(chkr_lseek, 19)
DO_SYSCALL(chkr_getpid, 20)
DO_SYSCALL_WITH_ERRNO(chkr_access, 33)
DO_SYSCALL_WITH_ERRNO(chkr_kill, 37)
DO_SYSCALL_WITH_ERRNO(chkr_ioctl, 54)
DO_SYSCALL_WITH_ERRNO(chkr_fstat, 62)
DO_SYSCALL_WITH_ERRNO(chkr_getpagesize, 64)
DO_SYSCALL_WITH_ERRNO(chkr_munmap, 73)
DO_SYSCALL_WITH_ERRNO(chkr_dup2, 90)
DO_SYSCALL_WITH_ERRNO(chkr_fcntl, 92)
DO_SYSCALL_WITH_ERRNO(chkr__sigvec, 108)
DO_SYSCALL_WITH_ERRNO(chkr_sigblock, 109)
DO_SYSCALL_WITH_ERRNO(chkr_sigsetmask, 110)
DO_SYSCALL_WITH_ERRNO(chkr_gettimeofday, 116)
DO_SYSCALL_WITH_ERRNO(chkr_ftruncate, 130)
DO_SYSCALL_WITH_ERRNO(chkr_sigpending, 183)
DO_SYSCALL_WITH_ERRNO(chkr_sysconf, 188)

