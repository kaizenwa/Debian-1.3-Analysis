#include <signal.h>

__sighandler_t
signal (int sig, __sighandler_t handler)
{
  int ret;
  struct sigaction action, oaction;
  action.sa_handler = handler;
  __sigemptyset (&action.sa_mask);
  action.sa_flags = SA_ONESHOT | SA_NOMASK | SA_INTERRUPT;
  action.sa_flags &= ~SA_RESTART;
  ret = __sigaction (sig, &action, &oaction); 
  return (ret == -1) ? SIG_ERR : oaction.sa_handler;
}

#include <gnu-stabs.h>
#ifdef elf_aliasb
elf_alias (signal, ssignal);
#endif
