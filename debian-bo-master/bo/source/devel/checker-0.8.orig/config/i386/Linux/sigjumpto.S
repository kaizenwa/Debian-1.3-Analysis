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

#ifdef __ELF__
#define C_NAME(x) x
#else
#define C_NAME(x) _##x
#endif

	.text
	.align 4
	.globl C_NAME(_sig_jump_to)
/* This function is called to deliver a signal. FUNC is the function to
 * call (the user function which catch the signal), and NEW_ESP points to
 * the first argument (ie:signal number). */
/* proto: void _sig_jump_to(void *func, int *new_esp); */
C_NAME(_sig_jump_to):
	pop %ecx 		# /* return addr > /dev/null */
	pop %edx		# /* %edx = FUNC */
	pop %eax		# /* %eax = NEW_ESP */
	movl -4(%eax), %ecx	# /* return addr of the signal. */
	movl %eax, %esp		# /* adjust the stack. */
	pushl %ecx		# /* the return addr */
	pushl %edx		# /* the function addr (cf ret) */

	pushl $0		# /* The return addr is unreadable. */
	pushl $4
	subl $4, %eax
	movl %eax, C_NAME(known_stack_limit)
	pushl %eax
	call C_NAME(chkr_set_right)
	addl $12, %esp
	
	movl 28(%esp), %edi	# /* read registers from sigcontext */
	movl 32(%esp), %esi
	movl 36(%esp), %ebp
	movl 44(%esp), %ebx
	movl 48(%esp), %edx
	movl 52(%esp), %ecx
	movl 56(%esp), %eax
	ret
#ifdef __ELF__
	.size C_NAME(_sig_jump_to), . - C_NAME(_sig_jump_to)
	.type C_NAME(_sig_jump_to), @function
#endif

#if 0

.lcomm tmp_esp, 4
.lcomm tmp_eip, 4
	.align 4
	.globl C_NAME(_sig_go_on)
/* The Checker handler has just catched a signal.  Since it can't be
 * delivered immidiately, it is saved and the execution goes on thanks to
 * this function. */
/* prototype: void _sig_go_on(struct sigcontext_struct); */
C_NAME(_sig_go_on):
	pop %eax		# /* return addr */
	pop %esi		# /* sigcontext */
	movl 16(%esi), %edi
	movl 24(%esi), %ebp
	movl 28(%esi), %eax
	movl %eax, tmp_esp
	movl 32(%esi), %ebx
	movl 36(%esi), %edx
	movl 40(%esi), %ecx
	movl 56(%esi), %eax
	movl %eax, tmp_eip
	movl 44(%esi), %eax
	pushl 64(%esi)
	popf
	movl 20(%esi), %esi
	movl tmp_esp, %esp
	pushl tmp_eip
	ret
#ifdef __ELF__
	.size C_NAME(_sig_go_on), . - C_NAME(_sig_go_on)
	.type C_NAME(_sig_go_on), @function
#endif


/*
 * This function call an user handler according to its args.
 * What is done can't be done in C: I have to call a function, but just
 *  before calling it, I must set the rights of its arguments.
 * It does nearly the same thing than this little function:
 */

#include "checker.h"

#######LEN is sizeof(STRUCT_SIG_CONTEXT);
void
send_sig_stub(void (*handler)(int), int nsignal, int len, STRUCT_SIG_CONTEXT *context)
{
  char tab[len];	### These 2 variables must be joined. 
  int sig;

  sig = nsignal;
  memcpy(tab, context, len);
  known_stack_limit = &sig;
  chkr_set_right(&sig, sizeof(int), CHKR_RW);
  chkr_set_right(tab, len, CHKR_RO);
  
  (*handler)();		#### The arg are SIG and TAB.
}

/* proto: void _send_sig_stub(void *handler, int nsignal, int len,
 *                          struct *context); */
	.align 4
	.globl C_NAME(_send_sig_stub)
C_NAME(_send_sig_stub):
	pushl %ebp		# /* Make the frame pointer. */
	movl %esp,%ebp

	pushl %edi		# /* Save some registers. */
	pushl %esi
	pushl %ebx	

	# /* allocate space for the args of the handler. */
	movl 16(%ebp),%eax	# /* len -> %eax */
	addl $4,%eax		# /* Add 4 bytes for the signal number. */
	subl %eax,%esp		# /* Allocate the space. */
	movl %esp,%ebx		# /* A new base pointer. */

	# /* Fill the args. */
	movl 12(%ebp),%esi	# /* Copy the signal number */
	movl %esi,(%ebx)

	movl 20(%ebp), %esi	# /* context -> %esi */
	movl 16(%ebp), %ecx	# /* len -> %ecx */
	leal 4(%ebx), %edi	# /* The destination. */
	shrl $2, %ecx		# /* Long word (4 bytes) are moved */
	cld
	rep
	movsl			# /* Copy the context. */

	# /* Set the rights of the args. */
	leal -4(%ebx), %edx
	movl %edx, C_NAME(known_stack_limit)	# /* Set the new stack limit. */

	pushl $0		# /* Make the return addr unreadable */
	pushl $4
	pushl %edx
	call C_NAME(chkr_set_right)

	pushl $3		# /* Make SIG readable and writable. */
	pushl $4
	pushl %ebx
	call C_NAME(chkr_set_right)

	pushl $1		# /* Make the context read only */
	movl 16(%ebp),%edx
	pushl %edx
	leal 4(%ebx), %edx
	pushl %edx
	call C_NAME(chkr_set_right)

	movl %ebx, %esp

	# /* Call the handler. */
	movl 8(%ebp),%edi
	call *%edi

	leal -12(%ebp),%esp	# /* Restore the registers */
	popl %ebx
	popl %esi
	popl %edi
	movl %ebp,%esp
	popl %ebp
	ret
#ifdef __ELF__
	.size C_NAME(_send_sig_stub), . - C_NAME(_send_sig_stub)
	.type C_NAME(_send_sig_stub), @function
#endif

#endif
