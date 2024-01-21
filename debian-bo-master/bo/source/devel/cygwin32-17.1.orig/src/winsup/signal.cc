/* signal for WIN32.

   Written by Steve Chamberlain of Cygnus Support.  
   sac@cygnus.com

   THIS SOFTWARE IS NOT COPYRIGHTED  

   Cygnus offers the following for use in the public domain.  Cygnus
   makes no warranty with regard to the software or it's performance
   and the user accepts the software "AS IS" with all faults.  

   CYGNUS DISCLAIMS ANY WARRANTIES, EXPRESS OR IMPLIED, WITH REGARD TO
   THIS SOFTWARE INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  
*/

#include "winsup.h"

_sig_func_ptr
signal (int sig, _sig_func_ptr func)
{
  _sig_func_ptr prev;

  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = signal (%d, %p)\n", sig, func);
      return (_sig_func_ptr) SIG_ERR;
    }

  prev = u->self->sigs[sig].sa_handler;
  u->self->sigs[sig].sa_handler = func;
  syscall_printf ("%p = signal (%d, %p)\n", prev, sig, func);
  return prev;
}

unsigned int
alarm (unsigned int)
{
  return 0;
}

unsigned int
sleep (unsigned int seconds)
{
  syscall_printf ("sleep (%d);\n", seconds);
  Sleep (seconds * 1000);
  syscall_printf ("0 = sleep (%d);\n", seconds);
  return 0;
}

unsigned int
usleep (unsigned int useconds)
{
  syscall_printf ("usleep (%d)\n", useconds);
  Sleep ((useconds + 500) / 1000);
  syscall_printf ("0 = usleep (%d)\n", useconds);
  return 0;
}

int
sigprocmask (int sig, const sigset_t *set, sigset_t *oldset)
{
  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigprocmask sig %d out of range\n", sig);
      return -1;
    }

  if (oldset)
    *oldset = u->self->sig_mask;
  if (set) 
    {
      switch (sig)
	{
	case SIG_BLOCK:
	  /* add set to current mask */
	  u->self->sig_mask |= *set;
	  break;
	case SIG_UNBLOCK:
	  /* remove set from current mask */
	  u->self->sig_mask &= ~*set;
	  break;
	case SIG_SETMASK:
	  /* just set it */
	  u->self->sig_mask = *set;
	  break;
	default:
	  set_errno (EINVAL);
	  return -1;
	}
    }
  return 0;
}

/* This is called _raise because the real raise is in newlib.  */
int
_raise (int sig)
{
  _sig_func_ptr p = u->self->sigs[sig].sa_handler;

  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = _raise (%d)\n", sig);
      return -1;
    }

  if (u->self->sig_mask & (1<<sig))
    return 0;

  /* Note that a blocked raise is sucessful.  */
  if (p == (_sig_func_ptr)SIG_DFL || p == (_sig_func_ptr)SIG_IGN
	|| p == (_sig_func_ptr)SIG_ERR)
    return 0;

  debug_printf ("calling the function at %x (%d)\n",
	       u->self->sigs[sig],sig);	       

  p (sig);
  return 0;
}

/* This is called _kill because the real kill is in newlib.  */
int
_kill (pid_t pid, int sig)
{
  int res = 0;
  pinfo  *dest = procinfo (pid);

  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigaction sig %d out of range\n", sig);
      return -1;
    }

  if (!dest)
    {
      set_errno (ESRCH);
      return -1;
    }
  if (sig != 0)
    {
      if (dest == this_procinfo ())
	{
	  res = _raise (sig);
	}
      else 
	{
	  int res = TerminateProcess (dest->hProcess, sig);
	  if (!res)
	    {
	      __seterrno ();
	      res = -1;
	    }
	}
    }
  syscall_printf ("%d = _kill (%d, %d);\n", res, pid, sig);
  return res;
}

int
sigaction (int sig,
		const struct sigaction *newaction,
		struct sigaction *oldaction)
{
  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigaction sig %d out of range\n", sig);
      return -1;
    }

  if (oldaction)
      *oldaction = u->self->sigs[sig];

  if (newaction)
    u->self->sigs[sig] = *newaction;

  return 0;
}

int
sigaddset (sigset_t *set, const int sig)
{
  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigaddset sig %d out of range\n", sig);
      return -1;
    }

  *set |= (1 << sig);
  return 0;
}
	       
int
sigdelset (sigset_t *set, const int sig)
{
  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigdelset sig %d out of range\n", sig);
      return -1;
    }

  *set &= ~(1 << sig);
  return 0;
}

int
sigismember (const sigset_t *set, int sig)
{
  /* check that sig is in right range */
  if (sig < 0 || sig >= NSIG)
    {
      set_errno (EINVAL);
      syscall_printf ("SIG_ERR = sigdelset sig %d out of range\n", sig);
      return -1;
    }

  if (*set & (1 << sig))
    return 1;
  else
    return 0;
}

int
sigemptyset (sigset_t * set)
{
  *set = (sigset_t) 0;
  return 0;
}

int
sigfillset (sigset_t * set)
{
  *set = ~((sigset_t) 0);
  return 0;
}

