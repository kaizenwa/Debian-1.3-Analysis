#include <signal.h>
#include <errno.h>

typedef (*signal_t)(void);

signal_t chkr_sigfunc[NSIG];
extern void chkr_sigtramp (void);
extern int chkr_sigblock (sigset_t);
extern int chkr__sigvec (int, struct sigvec *, struct sigvec *);
extern int chkr_sigsetmask (int);
extern int chkr_errno;

int
chkr_sigvec (int sig, struct sigvec *vec, struct sigvec *ovec)
{
  signal_t old_handler;
  int old_mask;
  
  if (sig <= 0 || sig > NSIG)
    {
      chkr_errno = EINVAL;
      return -1;
    }
  old_mask = chkr_sigblock (sigmask (sig));
  
  old_handler = chkr_sigfunc[sig];
  
  if (chkr__sigvec (sig, vec, ovec) < 0)
    {
      int saved_errno = chkr_errno;
      chkr_sigsetmask (old_mask);
      chkr_errno = saved_errno;
      return -1;
    }
  if (vec)
    {
      struct sigvec nvec = *vec;
      if (vec->sv_handler != SIG_DFL && vec->sv_handler != SIG_IGN)
        {
          chkr_sigfunc [sig] = vec->sv_handler;
          nvec.sv_handler = chkr_sigtramp;
        }
      if (chkr__sigvec (sig, &nvec, 0) < 0)
        {
          int saved_errno = chkr_errno;
          chkr_sigsetmask (old_mask);
          chkr_errno = saved_errno;
          chkr_sigfunc [sig] = old_handler;
          return -1;
        }
    }
  if (ovec)
    {
      if (ovec->sv_handler == chkr_sigtramp)
        ovec->sv_handler = old_handler;
    }
  chkr_sigsetmask (old_mask);
  return 0;
}
