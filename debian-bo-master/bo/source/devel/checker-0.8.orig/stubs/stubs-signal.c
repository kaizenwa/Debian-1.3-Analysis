/* Checker stubs for functions defined in signal.h
   Copyright 1995, 1996 Tristan Gingold
		  Written December 1995 by Tristan Gingold

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
#include "available-stubs.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#ifdef HAVE_SIGINFO_H
#include <siginfo.h>
#endif
#include "checker_api.h"

#undef HAVE_sigpending
#undef HAVE_sigsuspend

#if 0
#define HAVE_signal
#define HAVE_kill
#define HAVE_raise
#define HAVE_sigblock
#define HAVE_sigsetmask
#define HAVE_sigaddset
#define HAVE_sigdelset
#define HAVE_sigemptyset
#define HAVE_sigfillset
#define HAVE_sigismember
#define HAVE_siggetmask
#define HAVE_psignal
#define HAVE_sigpause
#define HAVE_sigprocmask
#endif

/* compiled from: . */
#ifdef HAVE_signal
typedef int (*sig_handler_type)();
sig_handler_type
chkr$signal (int sig, sig_handler_type handler)
{
#if USE_BI_JUMP
  __builtin_jump (signal);
#else
  {
    sig_handler_type res;
    res = signal (sig, handler);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_signal */

#ifdef HAVE_raise
int
chkr$raise (int sig)
{
#if USE_BI_JUMP
  __builtin_jump (raise);
#else
  return raise (sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_raise */

#ifdef HAVE_kill
int
chkr$kill (pid_t pid, int sig)
{
#if USE_BI_JUMP
  __builtin_jump (kill);
#else
  return kill (pid, sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_kill */

#ifdef HAVE_killpg
int
chkr$killpg (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (killpg);
#else
  {
    int res;
    res = killpg (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_killpg */

#ifdef HAVE_sigaddset
int
chkr$sigaddset (sigset_t *mask, int sig)
{
  stubs_chkr_check_addr (mask, sizeof (sigset_t), CHKR_RW, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigaddset);
#else
  return sigaddset (mask, sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigaddset */

#ifdef HAVE_sigdelset
int
chkr$sigdelset (sigset_t *mask, int sig)
{
  stubs_chkr_check_addr (mask, sizeof (sigset_t), CHKR_RW, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigdelset);
#else
  return sigdelset (mask, sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigdelset */

#ifdef HAVE_sigemptyset
int
chkr$sigemptyset (sigset_t *mask)
{
  stubs_chkr_check_addr (mask, sizeof (sigset_t), CHKR_WO, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigemptyset);
#else
  return sigemptyset (mask);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigemptyset */

#ifdef HAVE_sigfillset
int
chkr$sigfillset (sigset_t *mask)
{
  stubs_chkr_check_addr (mask, sizeof (sigset_t), CHKR_WO, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigfillset);
#else
  return sigfillset (mask);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigfillset */

#ifdef HAVE_sigismember
int
chkr$sigismember (const sigset_t *mask, int sig)
{
  stubs_chkr_check_addr (mask, sizeof (sigset_t), CHKR_RO, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigismember);
#else
  return sigismember (mask, sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigismember */

#ifdef HAVE_sigpending
int
chkr$sigpending (sigset_t * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (sigset_t), CHKR_XX, "mask");
#if USE_BI_JUMP
  __builtin_jump (sigpending);
#else
  {
    int res;
    res = sigpending (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigpending */

#ifdef HAVE_sigprocmask
int
chkr$sigprocmask (int how, const sigset_t *set, sigset_t *oset)
{
  if (set)
    stubs_chkr_check_addr (set, sizeof (sigset_t), CHKR_RO, "set");
  if (oset)
    stubs_chkr_check_addr (oset, sizeof (sigset_t), CHKR_WO, "oset");
#if USE_BI_JUMP
  __builtin_jump (sigprocmask);
#else
  return sigprocmask (how, (sigset_t *) set, oset);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigprocmask */

#ifdef HAVE_sigsuspend
int
chkr$sigsuspend (sigset_t * arg0)
{
  /* This function requires a stub */
  stubs_chkr_check_addr (arg0, sizeof (sigset_t), CHKR_XX, "set");
#if USE_BI_JUMP
  __builtin_jump (sigsuspend);
#else
  {
    int res;
    res = sigsuspend (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigsuspend */

#ifdef HAVE_sigaction
static void
stubs_chkr_check_sigaction (struct sigaction *s, int right)
{
  stubs_chkr_check_addr (&s->sa_handler, sizeof (s->sa_handler), right, "act->sa_handler");
  stubs_chkr_check_addr (&s->sa_mask, sizeof (s->sa_mask), right, "act->sa_mask");
  stubs_chkr_check_addr (&s->sa_flags, sizeof (s->sa_flags), right, "act->sa_flags");
}
  
int
chkr$sigaction (int sig, struct sigaction * act, struct sigaction * oldact)
{
  if (act)
    stubs_chkr_check_sigaction (act, CHKR_RO);
    
  /* OLDACT is optional... */
  if (oldact)
    stubs_chkr_check_sigaction (oldact, CHKR_MW);

#if USE_BI_JUMP
  __builtin_jump (sigaction);
#else
  return sigaction (sig, act, oldact);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigaction */

#ifdef HAVE_ssignal
__sighandler_t
chkr$ssignal (int arg0, __sighandler_t arg1)
{
#if USE_BI_JUMP
  __builtin_jump (ssignal);
#else
  {
    __sighandler_t res;
    res = ssignal (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_ssignal */

#ifdef HAVE_gsignal
int
chkr$gsignal (int arg0)
{
#if USE_BI_JUMP
  __builtin_jump (gsignal);
#else
  {
    int res;
    res = gsignal (arg0);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_gsignal */

#ifdef HAVE_sigblock
int
chkr$sigblock (int nsig)
{
#if USE_BI_JUMP
  __builtin_jump (sigblock);
#else
  return sigblock (nsig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigblock */

#ifdef HAVE_sigpause
int
chkr$sigpause (int sig)
{
#if USE_BI_JUMP
  __builtin_jump (sigpause);
#else
  return sigpause (sig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigpause */

#ifdef HAVE_sigsetmask
int
chkr$sigsetmask (int nsig)
{
#if USE_BI_JUMP
  __builtin_jump (sigsetmask);
#else
  return sigsetmask (nsig);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_sigsetmask */

#ifdef HAVE_siggetmask
int
chkr$siggetmask (void)
{
#if USE_BI_JUMP
  __builtin_jump (siggetmask);
#else
  return siggetmask ();
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_siggetmask */

#ifdef HAVE_psignal
void
chkr$psignal (int sig, const char *str)
{
  stubs_chkr_check_str (str, CHKR_RO, "str");
#if USE_BI_JUMP
  __builtin_jump (psignal);
#else
  psignal (sig, str);
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_psignal */

#ifdef HAVE_siginterrupt
int
chkr$siginterrupt (int arg0, int arg1)
{
#if USE_BI_JUMP
  __builtin_jump (siginterrupt);
#else
  {
    int res;
    res = siginterrupt (arg0, arg1);
    return res;
  }
#endif /* !USE_BI_JUMP */
}
#endif /* HAVE_siginterrupt */

#endif /* HAVE_SIGNAL_H */
