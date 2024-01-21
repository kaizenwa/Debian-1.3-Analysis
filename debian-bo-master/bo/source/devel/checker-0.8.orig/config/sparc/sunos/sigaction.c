#include <signal.h>
#include <errno.h>

extern int chkr_sigvec (int sig, struct sigvec *vec, struct sigvec *ovec);
extern int chkr_errno;

int
chkr_sigaction (int sig, struct sigaction *act, struct sigaction *oact)
{
  struct sigvec vec;
  struct sigvec ovec;
  struct sigvec *pvec;
  struct sigvec *povec;
  int res;
  
  if (sig <= 0 || sig >= NSIG || sig == SIGKILL || sig == SIGSTOP)
    {
      chkr_errno = EINVAL;
      return -1;
    }
  if (act)
    {
      vec.sv_handler = act->sa_handler;
      vec.sv_mask = act->sa_mask;
      vec.sv_flags = act->sa_flags;
      if (sig == SIGCHLD)
        vec.sv_flags &= ~SA_NOCLDSTOP;
      pvec = &vec;
    }
  else
    pvec = 0;
  
  if (oact)
    povec = &ovec;
  else
    povec = 0;
  
  res = chkr_sigvec (sig, pvec, povec);
  
  if (oact)
    {
      oact->sa_handler = ovec.sv_handler;
      oact->sa_mask = ovec.sv_mask;
      oact->sa_flags = ovec.sv_flags;
    }
  
  return res;
}
