/* 'Signal jumper' for Linux systems.
   Copyright 1994, 1995 Tristan Gingold
		  Written April 1994 by Tristan Gingold

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
#include <sys/syscall.h>

#ifdef __ELF__
#define C_NAME(x) x
#else
#define C_NAME(x) _##x
#endif

	.text
	.align 4
	.globl C_NAME(chkr_sigreturn)
/* An implementation of sigreturn for linux. */
/* proto: void _sigreturn(struct sigcontext_struct *context); */
/* sizeof(struct sigcontext_struct) = 23 * 4 */
C_NAME(chkr_sigreturn):
	pop %eax 		# /* return addr */
	pop %esi		# /* the arg */
	subl $(23 * 4), %esp
	movl %esp, %edi
	movl $23, %ecx
	cld
	rep
	movsl
	movl $(SYS_sigreturn), %eax
	int $0x80
	ret	/* stupid */
#ifdef __ELF__
	.size C_NAME(chkr_sigreturn), . - C_NAME(chkr_sigreturn)
	.type C_NAME(chkr_sigreturn), @function
#endif
