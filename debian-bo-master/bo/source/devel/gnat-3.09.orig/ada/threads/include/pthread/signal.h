/* Copyright (C) 1992, the Florida State University
   Distributed by the Florida State University under the terms of the
   GNU Library General Public License.

This file is part of Pthreads.

Pthreads is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation (version 2).

Pthreads is distributed "AS IS" in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied
warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with Pthreads; see the file COPYING.  If not, write
to the Free Software Foundation, 675 Mass Ave, Cambridge,
MA 02139, USA.

Report problems and direct all questions to:

  pthreads-bugs@ada.cs.fsu.edu

  @(#)signal.h	2.5 4/12/95

*/

#ifndef _pthread_signal_h
#define _pthread_signal_h

#ifndef	__signal_h

#ifdef LOCORE
#undef LOCORE
#endif

#ifdef SVR4_NP
#include <siginfo.h>
#include <ucontext.h>
#else !SVR4_NP
#include "stdtypes.h"
#endif !SVR4_NP

#include <signal.h>

#if !defined(__signal_h) && !defined(_SIGNAL_H) && !defined(__SIGNAL_H)
#define	__signal_h

#ifndef _sys_signal_h
typedef unsigned int sigset_t;
#endif

struct sigaction {    
        void            (*sa_handler)();
        sigset_t        sa_mask;
        int             sa_flags;
};
#endif __signal_h

#define NNSIG     NSIG+1

#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME 0

struct timespec {
  time_t tv_sec;
  long   tv_nsec;
};
#endif !CLOCK_REALTIME

#define PTHREAD_INTR_ENABLE       SIG_UNBLOCK
#define PTHREAD_INTR_DISABLE      SIG_BLOCK
#define PTHREAD_INTR_CONTROLLED   0
#define PTHREAD_INTR_ASYNCHRONOUS 1

#ifdef si_value
#undef si_value
#endif

union p_sigval {
  int sigval_int;
  void *sigval_prt;
};

struct p_siginfo {
  int si_signo;
  int si_code;
  union p_sigval si_value;
};

/*
 * This defines the implementation-dependent context structure provided
 * as the third parameter to user handlers installed by sigaction().
 * It should be a copy of the first part of the BSD sigcontext structure.
 * The second half should not be accessed since it is only present if
 * a _sigtramp instance is present right below the user handler on the
 * thread's stack. For SVR4, we will have to build this structure from scratch.
 */

#ifdef SOLARIS_NP

struct context_t {
  u_long  sc_flags;
  struct ucontext *sc_link;
  sigset_t sc_mask;  /* per-thread signal mask to be restored */
  stack_t  sc_stack;
  int sc_filler;
  greg_t  sc_psr;
  greg_t  sc_pc;     /* program counter to be restored */
  greg_t  sc_npc;    /* next pc (see below) */
  greg_t  sc_y;
  greg_t  sc_g1;
  greg_t  sc_g2;
  greg_t  sc_g3;
  greg_t  sc_g4;
  greg_t  sc_g5;
  greg_t  sc_g6;
  greg_t  sc_g7;
  greg_t  sc_o0;
  greg_t  sc_o1;
  greg_t  sc_o2;
  greg_t  sc_o3;
  greg_t  sc_o4;
  greg_t  sc_o5;
  greg_t  sc_sp;     /* stack pointer to be restored */
  greg_t  sc_o7;
};

#else !SOLARIS_NP
#ifdef SVR4_NP

struct context_t {
  This needs to be filled out!
  };

#else !SVR4_NP

#define p_sigval sigval
#define siginfo p_siginfo
typedef struct siginfo siginfo_t;

typedef int     greg_t;

struct context_t {
  greg_t sc_onstack;/* ignored */
  sigset_t sc_mask; /* per-thread signal mask to be restored */
  greg_t sc_sp;     /* stack pointer to be restored */
  greg_t sc_pc;     /* program counter to be restored */
  greg_t sc_npc;    /* next pc, only used if _sigtramp present
                     * on thread's stack, ignored o.w.
		     * should usually be pc+4
		     */
  greg_t sc_g1;
  greg_t sc_o0;
};

#endif !SVR4_NP
#endif !SOLARIS_NP

#ifndef SA_SIGINFO
#define SA_SIGINFO 0
#endif

#ifndef SA_ONSTACK
#define SA_ONSTACK SV_ONSTACK
#endif !SA_ONSTACK

#ifndef BUS_OBJERR
#define BUS_OBJERR FC_OBJERR
#endif !BUS_OBJERR

#ifndef BUS_CODE
#define BUS_CODE(x) FC_CODE(x)
#endif

#endif	__signal_h

#endif /*!_pthread_signal_h*/
