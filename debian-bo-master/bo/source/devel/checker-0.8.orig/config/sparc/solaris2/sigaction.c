/* sigaction() function.
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
#include <ucontext.h>
#include <errno.h>


typedef void (*_sighandler_t)(int, siginfo_t *, ucontext_t *);

int chkr___sigaction (int s, const struct sigaction *n, struct sigaction *o);
void chkr__setcontext (ucontext_t *ucp);

static _sighandler_t _siguhandler[NSIG];

static void
sigacthandler (int sig, siginfo_t *info, ucontext_t *context)
{
  (*(_siguhandler[sig])) (sig, info, context);
  
  if (sig == SIGFPE && context->uc_mcontext.fpregs.fpu_qcnt)
    {
       int i;
       struct fq *org;
       struct fq *dest;
       
       context->uc_mcontext.fpregs.fpu_qcnt--;
       dest = context->uc_mcontext.fpregs.fpu_q;
       org = dest + 1;
       for (i = 0; i < context->uc_mcontext.fpregs.fpu_qcnt; i++)
         dest[i] = org [i];
    }
#if 0
  chkr_printf ("Call setcontext\n");
#endif
  chkr__setcontext (context);
}

int
chkr_sigaction (int sig, const struct sigaction *nact, struct sigaction *oact)
{
  struct sigaction action;
  int res;
  
  if (sig <= 0 || sig > NSIG)
    return EINVAL;
    
  if (nact && (_sighandler_t)nact->sa_sigaction != (_sighandler_t)SIG_DFL
           && (_sighandler_t)nact->sa_sigaction != (_sighandler_t)SIG_IGN)
    {
      action = *nact;
      action.sa_sigaction = (void*)sigacthandler;
      res = chkr___sigaction (sig, &action, oact);
    }
  else
    {
#ifndef SOLARIS24_BUG_COMPACT
      if (oact)
        {
          res = chkr___sigaction (sig, nact, &action);
          *oact = action;
        }
      else
#endif
        res = chkr___sigaction (sig, nact, oact);
    }
  
  if (res == -1)
    return chkr_errno;

  if (oact && (_sighandler_t)oact->sa_sigaction != (_sighandler_t)SIG_DFL
           && (_sighandler_t)oact->sa_sigaction != (_sighandler_t)SIG_IGN)
    (_sighandler_t)oact->sa_sigaction = _siguhandler[sig];

  if (nact && (_sighandler_t)nact->sa_sigaction != (_sighandler_t)SIG_DFL
           && (_sighandler_t)nact->sa_sigaction != (_sighandler_t)SIG_IGN)
    _siguhandler[sig] = (_sighandler_t)nact->sa_sigaction;

  return 0;  
}
