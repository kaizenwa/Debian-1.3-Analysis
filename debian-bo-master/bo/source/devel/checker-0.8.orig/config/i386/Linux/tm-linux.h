/* Some systems dependant definitions
   Copyright 1993, 1994, 1995 Tristan Gingold
		  Written September 1993 by Tristan Gingold

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

/* See parse-args.c */
#if defined(ASCHECKER) && !defined(__ELF__)
#define NEED_CHKR_LD_OPTIONS
#endif

#ifdef ASCHECKER
/* This is done in codecheck.S.  */
#define ENTER_CHECKER
#define LEAVE_CHECKER

/* Signals are only delayed if we are in Checker.  */
#define DELAY_SIGNAL_CONDITION chkr_in_checker != 0

/* Save the signal: it will be emitted later.  On linux, we add it to a
   sig-set, and we will call kill later. */
#define SAVE_THIS_SIGNAL __sigaddset ((sigset_t *) &chkr_pending_signals, nsignal), 1

/* The pending signals that must be deliver when we are not in Checker. */
#define DECL_FOR_SAVE_THIS_SIGNAL volatile sigset_t chkr_pending_signals
#endif

/* The beginning of the '.text' segment.  */
#define TEXT_BASE 0x8000000

/* A way to set known_stack_limit */
#ifdef GCCCHECKER
#define SET_KNOWN_STACK_LIMIT known_stack_limit = __builtin_frame_address(1)
#endif

/* The current ip (for the checked instruction).  */
#define GET_CURRENT_IP(X) chkr_get_history (&X, 0, -1)

/* The stack base, ie where the stack begins. */
#define STACK_BASE 0xc0000000

/* Define STACK_GROWS_DOWNWARD is the stack grows downward. */
#define STACK_GROWS_DOWNWARD 1

/* The page size.  Note: CHKR_PAGESIZE = 1 << LOG_PAGESIZE. */
#define CHKR_PAGESIZE 0x1000
#define LOG_PAGESIZE 12

/* Linux supports shared memory.  */
#define HAVE_SHM

/* The high address available for the process.  Memory above this is reserved
 * by Checker (or by the system). */
#define HIGH_ADDR_HEAP 0x7fffffff

/* The biggest alignment on this machine: it must be convient for each type. */
#define BIGGEST_ALIGNMENT 4

/* To include struct sigcontext_struct. */
#include <asm/sigcontext.h>

/* The number of available signals. */
#define NSIGNALS _NSIG

/* The type of an handler. */
#define SIG_HANDLER_T __sighandler_t

/* The arguments of `chkr_sig_handler'.  `int nsignal' is already defined. */
#define OTHER_SIG_HANDLER_ARGS struct sigcontext_struct context

/* This macro modifies the handler context, so that SIG would be blocked
 * at the end of this handler.  Note that `sigblock()' doesn't work, because
 * the sigmask is restore when the handler exits. */
#define BLOCK_SIGNAL_WHEN_RETURN(sig) __sigaddset(&context.oldmask, sig)

/* Call the user handler FUNC with the args of chkr_sig_handler. */
#define SIG_JUMP_TO(func) _sig_jump_to ((PTR)func, &nsignal)
void _sig_jump_to (PTR, int *);

/* Set the rights for the handler args, so that the user handler would be able
 * to access them. */
#define SET_RIGHTS_FOR_HANDLER_ARGS \
  chkr_set_right((PTR)&nsignal, sizeof(int), CHKR_RW); \
  chkr_set_right((PTR)&context, sizeof(struct sigcontext_struct), CHKR_RO)

/* MAP_ANONYMOUS is available on Linux.  */
#define HAVE_ANONYMOUS

/* Define this if `chkr_do_end' must be registered via atexit().  With ELF,
 *  we do this with the .fini section.
 * This is necessary only for mdchecker and plchecker.
 */
#ifdef MDCHECKER
#define NEED_ATEXIT_FOR_CHKR_DO_END
#endif

/* If libchecker.o is used, incoming args of malloc, free, calloc... must be
 * checked.  This is possible/needed because they are on the stack.  The user
 * could break Checker with: w = malloc(); (no args).  */
#if !defined (MDCHECKER) && !defined (GCCCHECKER)
#define CHECK_INCOMING_ARGS
#endif

/* Linux a.out libraries can be used.  */
#define LINUX_AOUT_LIBRARIES

#ifdef NEED_MM
#include <sys/mman.h>
#include <linux/mman.h>
#ifndef MAP_FILE
#define MAP_FILE 0
#endif
#define MM_PROT		PROT_READ | PROT_WRITE
#define MM_FLAGS	MAP_FIXED | MAP_PRIVATE | MAP_ANONYMOUS
#define MM_FILE		(-1)

/* Between MM_LOW and MM_HIGH, the user can't access */
#define MM_LOW		0x80000000
#define MM_HIGH		0xaeffffff

/* Memory above MM_HEAP is used by sys_malloc (ie the internal heap, used only
 *  Checker). */
#define MM_HEAP		0x80000000	/* 512 Mb */

/* Where the stack bitmap begins. */
#define MM_STACK	0xa4000000	/* 64 Mb */

/* Where the bitmap for heaps begins. */
#define MM_MEM		0xa8000000	/* 64 Mb */

/* Where the symbol table is loaded. */
#define MM_SYM		0xac000000	/* 64 Mb */

#endif /* NEED_MM */
