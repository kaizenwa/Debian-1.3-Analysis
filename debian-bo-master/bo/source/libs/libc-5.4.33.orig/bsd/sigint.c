#include <signal.h>
#include <errno.h>

/* Set `sig' bit if we allow interrupt on it. */ 
sigset_t _sigintr = 0;

int
siginterrupt (int sig, int flag)
{
  struct sigaction sa;

  if (sig < 1 || sig >= NSIG) {
    errno = EINVAL;
    return -1;
  }

  if (__sigaction (sig, (struct sigaction *)0, &sa))
    return -1;
 
  if (flag) {
    __sigaddset (&_sigintr, sig);
#ifdef SA_RESTART
    if (!(sa.sa_flags & SA_RESTART) && (sa.sa_flags & SA_INTERRUPT))
      return 0;
    sa.sa_flags |= SA_INTERRUPT;
    sa.sa_flags &= ~ SA_RESTART;
#else
    if (sa.sa_flags & SA_INTERRUPT) return 0;
    sa.sa_flags |= SA_INTERRUPT;
#endif
  }
  else {
    __sigdelset (&_sigintr, sig);
#ifdef SA_RESTART
    if ((sa.sa_flags & SA_RESTART) && !(sa.sa_flags & SA_INTERRUPT))
      return 0;
    sa.sa_flags &= ~ SA_INTERRUPT;
    sa.sa_flags |= SA_RESTART;
#else
    if (!(sa.sa_flags & SA_INTERRUPT)) return 0;
    sa.sa_flags &= ~ SA_INTERRUPT;
#endif
  }
  return __sigaction (sig, &sa, (struct sigaction *)0);
}
