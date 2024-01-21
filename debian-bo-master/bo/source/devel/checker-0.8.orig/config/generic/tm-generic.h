/* Some systems dependant definitions
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

/* The high address available for the process.  Memory above this is reserved
 * by Checker (or by the system). */
#define HIGH_ADDR_HEAP 0x7fffffff

/* The biggest alignment on this machine: it must be convient for each type. */
#define BIGGEST_ALIGNMENT 8

/* Disable the signal manager.  If a signal is received, the behavior is not
   defined...  */
#define NO_SIGNALS
#if 0
/* The number of available signals. */
#define NSIGNALS NSIG

/* Signals are always delayed. */
#define DELAY_SIGNAL_CONDITION 1

/* The arguments of `chkr_sig_handler'.  `int nsignal' is already defined. */
#include <ucontext.h>
#define OTHER_SIG_HANDLER_ARGS siginfo_t *info, ucontext_t *context

/* This macro modifies the handler context, so that SIG would be blocked
 * at the end of this handler.  Note that `sigblock()' doesn't work, because
 * the sigmask is restore when the handler exits. */
#define BLOCK_SIGNAL_WHEN_RETURN(sig) __sigaddset(&context->uc_sigmask, sig)

/* Save the signal: it will be emitted later.  On linux, we add it to a
 * sig-set, and we will call kill later. */
#define SAVE_THIS_SIGNAL save_signal (nsignal, info, context, &sig_tab[nsignal])

/* The pending signals that must be deliver when we are not in Checker. */
#define DECL_FOR_SAVE_THIS_SIGNAL int save_signal \
  (int nsignal, siginfo_t *info, ucontext_t *context, struct sigaction *act)

/* Call the user handler FUNC with the args of chkr_sig_handler. */
#define SIG_JUMP_TO(func) chkr_abort ()

/* Set the rights for the handler args, so that the user handler would be able
 * to access them. */
#define SET_RIGHTS_FOR_HANDLER_ARGS					\
  do									\
    {									\
      if (info)								\
        chkr_set_right((PTR)info, sizeof(siginfo_t), CHKR_RW);		\
      chkr_set_right((PTR)context, sizeof(ucontext_t), CHKR_RW);	\
    }									\
  while(0)
#endif

/* Define this if `chkr_do_end' must be registered via atexit().  With ELF,
    we do this with the .fini section.
   This is necessary only for mdchecker and plchecker.  */
/* #define NEED_ATEXIT_FOR_CHKR_DO_END */

/* Set the current stack limit.  */
#define SET_KNOWN_STACK_LIMIT known_stack_limit = __builtin_frame_address(1)

/* Do not check that the incoming argument are readable.  */
#undef CHECK_INCOMING_ARGS

/* We dont need to access to the executable.  */
#define DONT_NEED_EXECUTABLE

/* sbrk is not redifined.  */
#define DONT_DEFINE_SBRK

/* How to get the current ip.  */
#define GET_CURRENT_IP(X) chkr_get_history(&X, 0, -1)

#include <signal.h>
#ifndef __sigaddset
#define __sigaddset sigaddset
#endif
#ifndef __sigemptyset
#define __sigemptyset sigemptyset
#endif
#ifndef __sigdelset
#define __sigdelset sigdelset
#endif
#ifndef __sigfillset
#define __sigfillset sigfillset
#endif
#ifndef __sigismember
#define __sigismember sigismember
#endif

/* The following lines are automatically appended by make-machine.  */
