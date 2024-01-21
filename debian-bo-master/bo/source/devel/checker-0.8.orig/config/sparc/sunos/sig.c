/* Signals manager for sunos.
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
#include <signal.h>

/* Defined in signal.c.  True if we are in Checker.  This is rather fuzzy. */
extern int chkr_sig_catched;

/* Contains enough data to send data. */
struct sig_saved
{
  int signal;
  int code;
  struct sigcontext sc;
  char *addr;
  struct sigaction *action;
};

/* Maximum number of signals that could be delayed. */
#define NBR_SIG_SAVED NSIGNALS
static struct sig_saved sig_saved[NBR_SIG_SAVED];

/* Two indexes in sig_saved that creat a circular list. */
static int incoming_index;
static int outcoming_index;

/* Save a signal to be delayed.  Call by signal.c. */
int
save_signal (int nsignal, int code, struct sigcontext *scp, char *addr, struct sigaction *act)
{
  /* Save... */
  sig_saved[incoming_index].signal = nsignal;
  sig_saved[incoming_index].code = code;
  sig_saved[incoming_index].sc = *scp;
  sig_saved[incoming_index].addr = *addr;
  sig_saved[incoming_index].action = act;
  
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

/* Send a delayed signal.  Extract one in the list and send it.  */
void
send_signal (void)
{
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
         
         (*sig_saved[outcoming_index].action->sa_handler)(
         	sig_saved[outcoming_index].signal,
         	sig_saved[outcoming_index].code,
         	&sig_saved[outcoming_index].sc,
         	sig_saved[outcoming_index].addr);
         	
         break;
      }
           
     /* Next signal. */
     outcoming_index = (outcoming_index + 1) % NBR_SIG_SAVED;
     chkr_sig_catched--;
   }
}
