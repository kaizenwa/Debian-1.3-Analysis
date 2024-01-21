/* Signals manager for simchecker.
   This file is part of Checker.
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
#include "checker.h"
#include "instr.h"
#include <sys/ucontext.h>
#include <signal.h>
#include <poll.h>

/* Defined in signal.c.  True if we are in Checker.  This is rather fuzzy. */
extern int chkr_sig_catched;

/* Defined in run_sim.c.  The descriptor for the server.  0 if not used. */
extern int gdbserver_desc;

extern int sim_stop, sim_trap;

/* Contains enough data to send data. */
struct sig_saved
{
  int signal;
  siginfo_t info;
  ucontext_t context;
  struct sigaction *action;
  int flags;
};
#define FL_INFO (1 << 0)	/* info field is valid. */

/* Maximum number of signals that could be delayed. */
#define NBR_SIG_SAVED NSIGNALS
static struct sig_saved sig_saved[NBR_SIG_SAVED];

/* Two indexes in sig_saved that creat a circular list. */
static int incoming_index;
static int outcoming_index;

/* Create a context from regs[]. */
void sim_getcontext (ucontext_t *ucp);

/* Used to link contexts.  See ucontext(5). */
ucontext_t *context_list;

/* Save a signal to be delayed.  Call by signal.c. */
int
save_signal (int nsignal, siginfo_t *info, ucontext_t *context, struct sigaction *act)
{
  if (nsignal == SIGPOLL && gdbserver_desc)
    {
      struct pollfd fds;
      int err;
      char c;
      
      fds.fd = gdbserver_desc;
      fds.events = POLLIN | POLLHUP;
      err = poll (&fds, 1, 0);
#if 0
      chkr_printf ("POLL results: err = %d, revents = %d\n", err, fds.revents);
#endif
      if (err == 1 && fds.revents == POLLIN)
        {
          err = read (gdbserver_desc, &c, 1);
          if (err == 1 && c == 3)
            {
              sim_stop = 1;
              sim_trap = SIGINT;
            }
          else
            chkr_printf ("gdbserver: bad interrupt err = %d, c = %d (`%c')\n", err, c, c);
          return 0;
        }
      else if (err == 1)
        {
          /* FIXME. */
          chkr_printf ("gdbserver: gdb hangup. exit.\n");
          _exit (1);
        }
    }
    
  /* Save... */
  sig_saved[incoming_index].signal = nsignal;
  sig_saved[incoming_index].context = *context;
  sig_saved[incoming_index].action = act;
  
  if (info)
    {
      sig_saved[incoming_index].info = *info;
      sig_saved[incoming_index].flags = FL_INFO;
    }
  else
    sig_saved[incoming_index].flags = 0;
    
  /* Update the index. */
  incoming_index = (incoming_index + 1) % NBR_SIG_SAVED;
  
  /* I hope this would never happen. */
  if (outcoming_index == incoming_index)
    {
      chkr_printf ("Can't delay signals anymore.\n");
      chkr_abort ();
    }
  
  /* The signal will be blocked. */
  return 1;
}

/* Send a delayed signal.  Extract one in the list and send it.  Called by
 * instr.c(sim). */
void
send_signal (void)
{
 ucontext_t ucp;
 ucontext_t *uc;
 siginfo_t *si;
 
 if (outcoming_index != incoming_index)
   {
     /* Emit a warning. */
     emit_warning_for_signal (sig_saved[outcoming_index].signal);
     
     switch ((uint)sig_saved[outcoming_index].action->sa_handler)
       {
     case SIG_IGN:
     	 /* Ignore the signal: nothing to do.  Just display a warning. */
     	 break;
     case SIG_DFL:
         /* If GDB is active, don't accept the signal. */
         if (gdbserver_desc)
           {
             sim_stop = 1;
             sim_trap = sig_saved[outcoming_index].signal;
           }
         else
           {
             sigset_t set;
             struct sigaction action;
           
             /* Set the default behavior */
             action.sa_handler = SIG_DFL;
             chkr_sigaction (sig_saved[outcoming_index].signal, &action, &action);
     
             /* Unblock it */
             __sigfillset (&set);
             __sigdelset (&set, sig_saved[outcoming_index].signal);
             sigprocmask (SIG_SETMASK, &set, 0);
             kill (my_pid, sig_saved[outcoming_index].signal);
     
             /* Set the previous behavior if the signal is ignored by default. */
             chkr_sigaction (sig_saved[outcoming_index].signal, &action, 0);
           }
         break;
     default:
/*         verbose_flag = 2; */
         
         /* Set the mask defined by the handler. */
         sigprocmask (SIG_SETMASK, &(sig_saved[outcoming_index].action->sa_mask), 0);

         /* Save the context. */
         sim_getcontext (&ucp);
         ucp.uc_sigmask = sig_saved[outcoming_index].context.uc_sigmask;

         /* Save the old context on the stack. */
         regs[SP] -= sizeof (ucontext_t);
         uc = (ucontext_t*)regs[SP];
         memcpy (uc, &ucp, sizeof (ucontext_t));
         context_list = uc;

         /* Save the signal info. */
         if (sig_saved[outcoming_index].flags & FL_INFO)
           {
             regs[SP] -= sizeof (siginfo_t);
             si = (siginfo_t*)regs[SP];
             memcpy (si, &sig_saved[outcoming_index].info, sizeof (siginfo_t));
           }
         else
           si = (siginfo_t*) 0;
         
         regs[SP] -= 64;
         
         /* Set the pcs */
         pc = (uint)sig_saved[outcoming_index].action->sa_handler;
         npc = (uint)sig_saved[outcoming_index].action->sa_handler + 4;

         /* Set the args of the handler. */
         regs[O0] = sig_saved[outcoming_index].signal;
         regs[O1] = (uint) si;
         regs[O2] = (uint) uc;
         
	 /* Inform GDB. */
         if (gdbserver_desc)
           {
             sim_stop = 1;
             sim_trap = sig_saved[outcoming_index].signal;
           }
         break;
      }
           
     /* Next signal. */
     outcoming_index = (outcoming_index + 1) % NBR_SIG_SAVED;
     chkr_sig_catched--;
   }
}

/* Store the 'virtual' context into ucp. */
void
sim_getcontext (ucontext_t *ucp)
{
 ucp->uc_flags = UC_SIGMASK | UC_CPU | UC_FPU;
 sigprocmask (SIG_SETMASK, (sigset_t*)0, &(ucp->uc_sigmask));
 ucp->uc_link = context_list;
 ucp->uc_mcontext.gregs[REG_PSR] = * ((int*)&psr);
 ucp->uc_mcontext.gregs[REG_PC] = pc;
 ucp->uc_mcontext.gregs[REG_nPC] = npc;
 ucp->uc_mcontext.gregs[REG_Y] = y;
 ucp->uc_mcontext.gregs[REG_G1] = regs[G1];
 ucp->uc_mcontext.gregs[REG_G2] = regs[G2];
 ucp->uc_mcontext.gregs[REG_G3] = regs[G3];
 ucp->uc_mcontext.gregs[REG_G4] = regs[G4];
 ucp->uc_mcontext.gregs[REG_G5] = regs[G5];
 ucp->uc_mcontext.gregs[REG_G6] = regs[G6];
 ucp->uc_mcontext.gregs[REG_G7] = regs[G7];
 ucp->uc_mcontext.gregs[REG_O0] = regs[O0];
 ucp->uc_mcontext.gregs[REG_O1] = regs[O1];
 ucp->uc_mcontext.gregs[REG_O2] = regs[O2];
 ucp->uc_mcontext.gregs[REG_O3] = regs[O3];
 ucp->uc_mcontext.gregs[REG_O4] = regs[O4];
 ucp->uc_mcontext.gregs[REG_O5] = regs[O5];
 ucp->uc_mcontext.gregs[REG_O6] = regs[O6];
 chkr_printf ("getcontext: reg_O6 = 0x%08x\n", regs[O6]);
 ucp->uc_mcontext.gregs[REG_O7] = regs[O7];
 ucp->uc_mcontext.gwins = (gwindows_t*) 0;
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[0] = fregs.d[0];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[1] = fregs.d[1];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[2] = fregs.d[2];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[3] = fregs.d[3];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[4] = fregs.d[4];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[5] = fregs.d[5];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[6] = fregs.d[6];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[7] = fregs.d[7];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[8] = fregs.d[8];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[9] = fregs.d[9];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[10] = fregs.d[10];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[11] = fregs.d[11];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[12] = fregs.d[12];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[13] = fregs.d[13];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[14] = fregs.d[14];
 ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[15] = fregs.d[15];
 ucp->uc_mcontext.fpregs.fpu_q = (struct fq*) 0;
 ucp->uc_mcontext.fpregs.fpu_fsr = * ((uint*)&fpsr);
 ucp->uc_mcontext.fpregs.fpu_qcnt = 0;
 ucp->uc_mcontext.fpregs.fpu_q_entrysize = sizeof (struct fq);
 ucp->uc_mcontext.fpregs.fpu_en = 0;
}

/* Restore a virtual context. */
void
sim_setcontext (ucontext_t *ucp)
{
#if 0
 chkr_printf ("setcontext called (flags = 0x%08x, pc = 0x%08x)\n",
 	 (uint)ucp->uc_flags, (uint)ucp->uc_mcontext.gregs[REG_PC]);
#endif
 context_list = ucp->uc_link;
 
 if (ucp->uc_flags & UC_SIGMASK)
   sigprocmask (SIG_SETMASK, &(ucp->uc_sigmask), (sigset_t*)0);
  
 if (ucp->uc_flags & UC_CPU)
   {
     *((int*)&psr) = ucp->uc_mcontext.gregs[REG_PSR];
     pc = ucp->uc_mcontext.gregs[REG_PC];
     chkr_check_exec ((PTR)pc);
     npc = ucp->uc_mcontext.gregs[REG_nPC];
     chkr_check_exec ((PTR)npc);
     y = ucp->uc_mcontext.gregs[REG_Y];
     regs[G1] = ucp->uc_mcontext.gregs[REG_G1];
     regs[G2] = ucp->uc_mcontext.gregs[REG_G2];
     regs[G3] = ucp->uc_mcontext.gregs[REG_G3];
     regs[G4] = ucp->uc_mcontext.gregs[REG_G4];
     regs[G5] = ucp->uc_mcontext.gregs[REG_G5];
     regs[G6] = ucp->uc_mcontext.gregs[REG_G6];
     regs[G7] = ucp->uc_mcontext.gregs[REG_G7];
     regs[O0] = ucp->uc_mcontext.gregs[REG_O0];
     regs[O1] = ucp->uc_mcontext.gregs[REG_O1];
     regs[O2] = ucp->uc_mcontext.gregs[REG_O2];
     regs[O3] = ucp->uc_mcontext.gregs[REG_O3];
     regs[O4] = ucp->uc_mcontext.gregs[REG_O4];
     regs[O5] = ucp->uc_mcontext.gregs[REG_O5];
     regs[O6] = ucp->uc_mcontext.gregs[REG_O6];
     regs[O7] = ucp->uc_mcontext.gregs[REG_O7];
     /* FIXME: test the stack. */
#if 0
     ucp->uc_mcontext.gwins = (gwindows_t*) 0;
#endif
   }
 if (ucp->uc_flags & UC_FPU)
   {
     fregs.d[0] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[0];
     fregs.d[1] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[1];
     fregs.d[2] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[2];
     fregs.d[3] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[3];
     fregs.d[4] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[4];
     fregs.d[5] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[5];
     fregs.d[6] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[6];
     fregs.d[7] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[7];
     fregs.d[8] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[8];
     fregs.d[9] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[9];
     fregs.d[10] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[10];
     fregs.d[11] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[11];
     fregs.d[12] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[12];
     fregs.d[13] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[13];
     fregs.d[14] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[14];
     fregs.d[15] = ucp->uc_mcontext.fpregs.fpu_fr.fpu_dregs[15];
#if 0
     ucp->uc_mcontext.fpregs.fpu_q = (struct fq*) 0;
#endif
     *((uint*)&fpsr) = ucp->uc_mcontext.fpregs.fpu_fsr = * ((uint*)&fpsr);
#if 0
     ucp->uc_mcontext.fpregs.fpu_qcnt = 0;
     ucp->uc_mcontext.fpregs.fpu_q_entrysize = sizeof (struct fq);
     ucp->uc_mcontext.fpregs.fpu_en = 0;
#endif
  }
}
