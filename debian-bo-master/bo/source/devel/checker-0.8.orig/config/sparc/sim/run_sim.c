/* Sparc simulator starter.
   Copyright 1995 Tristan Gingold
		  Written Juny 1995 by Tristan Gingold

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

#include <sys/frame.h>
#include <signal.h>
#include "instr.h"
#ifdef SIMCHECKER
#include "checker.h"
#else
#include <unistd.h>
#include <stdlib.h>
#endif

/* The new stack used for the simulator. */
#define STACK_SIZE 4096
uint stack1[STACK_SIZE];	/* 16ko */
uint *stack;

void run1 (int sig);
void run_sim (void);
void disassemble (union Instr instr);

extern int flag_verbose;
extern int gdbserver_desc;
extern uint sim_trap;

#ifdef USE_CONTEXT
ucontext_t uc;
#else
struct sigaction oact;
#endif

/* Define this macro if you want info about contexts. */
#undef CONTEXT_VERBOSE

#ifdef CONTEXT_VERBOSE
static void disp_uc (ucontext_t *uc);
#endif

/* The entry point of the simulator:  it will transfert the execution to the
 * simulator.  The simulator will resume after this function.
 * Its purpose is to initialize the register and use another stack.
 */
void
run_sim (void)
{
 struct frame *res;
 uint *new_stack;
 char *gdbserver;
#ifndef USE_CONTEXT
 stack_t ss_stack;
 struct sigaction act;
#endif

#ifndef SIMCHECKER 
 /* Set verbose flag if SIM_VERBOSE is present. */
 if (getenv ("SIM_VERBOSE"))
   flag_verbose = 1;
 else
   flag_verbose = 0;

 if (getenv ("SIM_DISABLE"))
   return;
#endif

 /* If CHKRGDBSERVER is defined, it must be a number, which defines a fd.
  *  This fd is used to communicate with GDB.
  */
 gdbserver = getenv ("CHKRGDBSERVER");
 if (gdbserver)
   {
     gdbserver_desc = atod (gdbserver);
     if (gdbserver_desc)
       {
         int fd;
         
         /* Protect the fd.  This prevents the user to handle it. */
         fd = fd_alloc_reserved ("gdbserver");
         dup2 (gdbserver_desc, fd);
         gdbserver_desc = fd;
       }
   }
   
#if 0
 /* Used by sstep. */
 {
   time_t t;
   stime (&t);
 }
#endif
 
 /* The new stack. */
 stack = (uint*) (((uint)stack1 + CHKR_PAGESIZE - 1) & ~(CHKR_PAGESIZE - 1));
 new_stack = &stack[STACK_SIZE - 26];	/* 28 = 112 / 2 */
 
 /* Set some registers. */
 asm volatile (
	"ta 0x3\n\t"
	"mov %%sp, %0\n\t" : "=r"(res));
 regs[I0] = res->fr_arg[0];
 regs[I1] = res->fr_arg[1];
 regs[I2] = res->fr_arg[2];
 regs[I3] = res->fr_arg[3];
 regs[I4] = res->fr_arg[4];
 regs[I5] = res->fr_arg[5];
 regs[I6] = (uint)res->fr_savfp;
 regs[I7] = res->fr_savpc;
 regs[SP] = (uint)res;
 
#ifdef USE_CONTEXT
 /* We have to use another stack for the simulator.  The simulator can't use
  * the standard stack to avoid clash.  It is too difficult to remap the
  * standard stack.  So we have to create our own stack.  On Solaris, we
  * can't just set SP: we have to create a new context. */
 uc.uc_flags = 7;
 chkr___getcontext (&uc);
#if CONTEXT_VERBOSE
 chkr_printf ("Raw context:\n");
 disp_uc (&uc);
#endif
 uc.uc_link = &uc + 1;
 uc.uc_stack.ss_size = 0x2000; /*(uint)stack1 + sizeof (stack1) - (uint)stack; */
 uc.uc_stack.ss_sp = (char*)stack;
 uc.uc_stack.ss_flags = 0;
 uc.uc_mcontext.gregs[REG_PC] = (uint)run1;
 uc.uc_mcontext.gregs[REG_nPC] = (uint)run1 + 4;
 uc.uc_mcontext.gregs[REG_O0] = 57;
 
 /* Align the stack: 8 bytes.
  * Some comments about that:
  *  why ? (I think the OS can use LDD to dump faster).
  *  where is it documented ?
  */
 uc.uc_mcontext.gregs[REG_O6] = (uint)stack + 0x1f00; /*(uint)new_stack & ~7; */
 
 /* Set the context.  Must not return. */
#if CONTEXT_VERBOSE
 chkr_printf ("\nWanted context:\n");
 disp_uc (&uc);
#endif
 chkr__setcontext (&uc);
#else
 ss_stack.ss_size = 0x2000;
 ss_stack.ss_sp = (char*)stack;
 ss_stack.ss_flags = 0;
 sigaltstack (&ss_stack, 0);
 
 act.sa_handler = run1;
 sigemptyset (&act.sa_mask);
 act.sa_flags = SA_ONSTACK;
 chkr_sigaction (SIGUSR1, &act, &oact);
 kill (getpid (), SIGUSR1);
#endif

 chkr_abort ();
}

#ifdef CONTEXT_VERBOSE
static void
disp_ss (stack_t *ss)
{
  chkr_printf ("ss_sp:    0x%08x\n", (int)ss->ss_sp);
  chkr_printf ("ss_size:  0x%08x\n", ss->ss_size);
  chkr_printf ("ss_flags: 0x%08x\n", ss->ss_flags);
}

static void
disp_uc (ucontext_t *uc)
{
 chkr_printf ("uc_link:  0x%08x\n", (uint)uc->uc_link);
 chkr_printf ("uc_flags: 0x%08x\n", (uint)uc->uc_flags);
 disp_ss (&(uc->uc_stack));
 chkr_printf ("uc_mcontext.gregs[REG_O6]: 0x%08x\n", uc->uc_mcontext.gregs[REG_O6]);
 chkr_printf ("uc_mcontext.gregs[REG_PC]: 0x%08x\n", uc->uc_mcontext.gregs[REG_PC]);
 chkr_printf ("uc_mcontext.gregs[REG_nPC]: 0x%08x\n", uc->uc_mcontext.gregs[REG_nPC]);
}
#endif /* CONTEXT_VERBOSE */

/* Here is the fork that can be sumed up with this scheme:
 *
 * --------> run_sim() --------> run1() +--------------> sim() really executed
 *        \                             |                      by the CPU.
 *         +---       <-----------------+
 *           This is executed by the simulator.
 */ 
void
run1 (int sig)
{
  stack_t ss;
#if CONTEXT_VERBOSE
  ucontext_t ucp;
  chkr_printf ("\nrun 1:\n");
  chkr___getcontext (&ucp);
  sigaltstack (0, &ss);
  chkr_printf ("New context:\n");
  disp_uc (&ucp);
  disp_ss(&ss);
#endif
  
  /* Just set PC and nPC.
   * they will return. */
  asm volatile (
  	"call 1f\n\t"
  	"nop\n\t"
  	"ret\n\t"
  	"restore\n"
  	"1:\tadd %%o7, 8, %0\n\t"
  	"add %%o7, 12, %1\n\t"
  	: "=r" (pc), "=r"(npc));

#ifndef USE_CONTEXT
  chkr_sigaction (SIGUSR1, &oact, 0);
  ss.ss_size = 0;
  ss.ss_sp = 0;
  ss.ss_flags = SS_DISABLE;
  sigaltstack (&ss, 0);
#endif

  /* Call the simulator. */
  if (gdbserver_desc)
    gdb_server_sim ();
    
  while (1)
    {
      union Instr instr;
      
      sim (1);
      instr.word = *((uint*)pc);
      disassemble (instr);
      kill (my_pid, sim_trap);
    }
}

#ifndef SIMCHECKER
#ifdef __PIC__
asm (".section \".init\"\n"
    "\t.type _init,#function\n"
    "\t.globl _init\n"
    "_init:\tsave %sp, -96, %sp\n"
    "call run\n"
    "\tnop\n"
    "\tret\n"
    "\trestore\n"
    "\t.size _init,.-_init\n");
#endif
#endif
