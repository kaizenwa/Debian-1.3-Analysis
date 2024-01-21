#ifdef __ELF__
#ifdef __GNUC__
int shutdown (int, int)  __attribute__ ((weak));
#else
#pragma weak shutdown
#endif   /* __GNUC__ */
#endif   /* __ELF__ */

#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

#ifdef __SVR4_I386_ABI_L1__
#define socketcall __socketcall
#else
static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);
#endif

/* shutdown by bir7@leland.stanford.edu */

int
shutdown (int sockfd, int how)
{
  unsigned long args[2];
  args[0] = sockfd;
  args[1] = how;
  return (socketcall (SYS_SHUTDOWN, args));
}
