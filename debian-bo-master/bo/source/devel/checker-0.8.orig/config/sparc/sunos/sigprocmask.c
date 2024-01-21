#define sigblock chkr_sigblock
#define sigsetmask chkr_sigsetmask
#include <signal.h>

int
chkr_sigprocmask (int how, sigset_t *set, sigset_t *oset)
{
  int omask;
  if (how == SIG_BLOCK || how == SIG_UNBLOCK || oset)
    omask = sigblock (0);
  if (oset)
    *oset = omask;
  if (set)
    {
      if (how == SIG_BLOCK)
        return sigblock (omask | *set);
      else if (how == SIG_UNBLOCK)
        return sigsetmask (omask & ~*set);
      else if (how == SIG_SETMASK)
        return sigsetmask (*set);
      else
        return -1;
    }
  return 0;
}
