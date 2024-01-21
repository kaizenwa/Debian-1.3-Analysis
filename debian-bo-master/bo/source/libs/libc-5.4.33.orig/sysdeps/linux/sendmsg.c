#ifdef __ELF__
#ifdef __GNUC__
struct msghdr;
int sendmsg (int, const struct msghdr *, unsigned) __attribute__ ((weak));
#else
#pragma weak sendmsg
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

int
sendmsg (int sockfd, const struct msghdr *msg, unsigned flags)
{
  unsigned long args[3];
  args[0] = sockfd;
  args[1] = (unsigned long) msg;
  args[2] = flags;
  return (socketcall (SYS_SENDMSG, args));
}
