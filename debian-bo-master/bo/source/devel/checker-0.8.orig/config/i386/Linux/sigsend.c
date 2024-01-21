/* Catch and send signals for Linux systems.
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
#include "checker.h"
#include "errlist.h"

#if 0 /* Done in checker.h */
/* True if a signal is being delayed.  See parse-args.c.  */
extern int chkr_sig_catched;
#endif

/* Signals delayed. */ 
extern volatile int chkr_pending_signals;

/* This structure is hard-coded and defined in config/i386/codecheck.S.  */
struct context
{
  long eax;
  long ecx;
  long edx;
  long ebx;
  long esp;
  long ebp;
  long esi;
  long edi;
  long eip;
  long flags;
  /* Don't forget these !!! */
  long size;
  long right;
  long accmod;
  long addr;
};

/* Defined in codecheck.S.  */
extern struct context linux_context;

/* Deliver signals.  Called by codecheck.S(restore_register).  */
void
chkr_send_delayed_signals (void)
{
 sigset_t set;
 int n;
 struct context saved_context = linux_context;
 
 if (!chkr_pending_signals)
   {
     chkr_printf ("No signals to deliver in chkr_send_delayed_signals\n");
     chkr_abort ();
   }
   
 while (chkr_pending_signals)
   {
     n = ffs (chkr_pending_signals);
     __sigdelset (&chkr_pending_signals, n);
     chkr_sig_catched--;
     __sigemptyset (&set);
     __sigaddset (&set, n);
     sigprocmask (SIG_UNBLOCK, &set, 0);
#if 0 && defined (ASCHECKER)
     chkr_printf ("Emit delayed signal #%d, eip:%08x ebp: %08x\n", n, context->eip, context->ebp);
#endif
     if (kill (my_pid, n) != 0)
       {
         chkr_printf ("Fail to send a signal to myself.\n");
       }
   }

 linux_context = saved_context;

 return;    
}
