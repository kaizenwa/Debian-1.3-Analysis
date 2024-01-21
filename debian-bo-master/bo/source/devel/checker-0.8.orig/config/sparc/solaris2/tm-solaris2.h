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

#ifdef SIMCHECKER
#define ENTER_CHECKER
#define LEAVE_CHECKER
#endif

/* The stack base, ie where the stack begins. */
#define STACK_BASE 0xf0000000

/* Define STACK_GROWS_DOWNWARD is the stack grows downward. */
#define STACK_GROWS_DOWNWARD 1

/* The page size.  Note: CHKR_PAGESIZE = 1 << LOG_PAGESIZE. */
#undef CHKR_PAGESIZE		/* this is defined in sys/param.h */
#define CHKR_PAGESIZE pagesize
#define LOG_PAGESIZE log_pagesize
#define INIT_PAGESIZE sysconf(_SC_PAGESIZE)

/* Solaris2 supports shared memory.  */
#define HAVE_SHM

/* The high address available for the process.  Memory above this is reserved
 * by Checker (or by the system). */
#define HIGH_ADDR_HEAP 0x7fffffff

/* The biggest alignment on this machine: it must be convient for each type. */
#define BIGGEST_ALIGNMENT 8

/* The number of available signals. */
#define NSIGNALS NSIG

#ifdef SIMCHECKER
/* Signals are always delayed. */
#define DELAY_SIGNAL_CONDITION 1

/* Save the signal: it will be emitted later.  */
#define SAVE_THIS_SIGNAL save_signal (nsignal, info, context, &sig_tab[nsignal])

/* The pending signals that must be deliver when we are not in Checker. */
#define DECL_FOR_SAVE_THIS_SIGNAL int save_signal \
  (int nsignal, siginfo_t *info, ucontext_t *context, struct sigaction *act)
#endif

/* The arguments of `chkr_sig_handler'.  `int nsignal' is already defined. */
#include <ucontext.h>
#define OTHER_SIG_HANDLER_ARGS siginfo_t *info, ucontext_t *context

/* This macro modifies the handler context, so that SIG would be blocked
 * at the end of this handler.  Note that `sigblock()' doesn't work, because
 * the sigmask is restore when the handler exits. */
#define BLOCK_SIGNAL_WHEN_RETURN(sig) __sigaddset(&context->uc_sigmask, sig)

#ifdef GCCCHECKER
/* Call the user handler FUNC with the args of chkr_sig_handler. */
#define SIG_JUMP_TO(func) (*func)(nsignal, info, context)
#endif

#ifdef SIMCHECKER
#define SIG_JUMP_TO(func) chkr_abort ()
#endif

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

/* Define this if `chkr_do_end' must be registered via atexit().  With ELF,
 *  we do this with the .fini section.
 * This is necessary only for mdchecker and plchecker
 */
/* #define NEED_ATEXIT_FOR_CHKR_DO_END */

/* Add ld.so in the link_map structure.  */
#define ADD_LD_SO_IN_OBJECTS

/* This is for the simulator. */
#ifdef SIMCHECKER
#define malloc stub_malloc
#define calloc stub_calloc
#define valloc stub_valloc
#define realloc stub_realloc
#define free stub_free
#define memalign stub_memalign
#define sbrk stub_sbrk
#ifndef SP
#define SP 14
#endif
extern uint regs[32];
extern uint pc;
#define SET_KNOWN_STACK_LIMIT known_stack_limit = (PTR)regs[SP]
#define GET_CURRENT_IP(X) X = (PTR) pc
#endif

#ifdef GCCCHECKER
#define GET_CURRENT_IP(X) chkr_get_history (&X, 0, -1)
#define SET_KNOWN_STACK_LIMIT known_stack_limit = __builtin_frame_address(1)
#endif

#undef CHECK_INCOMING_ARGS

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

/* ANONYMOUS is not available.  Use devzero_fd. */
#undef HAVE_ANONYMOUS

#ifdef NEED_MM
#include <sys/mman.h>
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
#define MM_PROT		PROT_READ | PROT_WRITE
#define MM_FLAGS	MAP_FIXED | MAP_PRIVATE
#define MM_FILE		devzero_fd

/* Between MM_LOW and MM_HIGH, the user can't access */
#define MM_LOW		0xe0000000
#define MM_HIGH		0xeeffffff

/* Memory above MM_HEAP is used by sys_malloc (ie the internal heap, used only
 *  Checker). */
#define MM_HEAP		0xe0000000	/* 64 Mb */

/* Where the stack bitmap begins. */
#define MM_STACK	0xe4000000	/* 64 Mb */

/* Where the bitmap for heaps begins. */
#define MM_MEM		0xe8000000	/* 64 Mb */

/* Where the symbol table is loaded. */
#define MM_SYM		0xec000000	/* 64 Mb */

#endif /* NEED_MM */
