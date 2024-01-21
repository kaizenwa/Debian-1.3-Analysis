#ifdef __ELF__
int recvmsg() __attribute__((weak));
#endif

#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

#ifdef __SVR4_I386_ABI_L1__
#define socketcall __socketcall
#else
static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);
#endif

int
recvmsg (int sockfd, struct msghdr *msg, unsigned flags)
{
  unsigned long args[3];
  args[0] = sockfd;
  args[1] = (unsigned long) msg;
  args[2] = flags;
  return (socketcall (SYS_RECVMSG, args));
}
