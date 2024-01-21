#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

#ifdef __SVR4_I386_ABI_L1__
#define socketcall __socketcall
#else
static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);
#endif

/* recv, recvfrom added by bir7@leland.stanford.edu */

int
recv (int sockfd, void *buffer, size_t len, unsigned flags)
{
  unsigned long args[4];
  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  return (socketcall (SYS_RECV, args));
}

#ifdef __ELF__
#ifdef __GNUC__
int recv (int, void *, size_t, unsigned) __attribute__ ((weak));
#else
#pragma weak recv
#endif   /* __GNUC__ */
#endif   /* __ELF__ */
