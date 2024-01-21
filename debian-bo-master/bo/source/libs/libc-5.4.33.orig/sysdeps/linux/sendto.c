#ifdef __ELF__
#ifdef __GNUC__
#include <stddef.h> /* for size_t */
struct sockaddr;
int sendto (int, const void *, size_t, unsigned, const struct sockaddr *, int)
   __attribute__ ((weak));
#else
#pragma weak sendto
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

/* send, sendto added by bir7@leland.stanford.edu */

int
sendto (int sockfd, const void *buffer, size_t len, unsigned flags,
	const struct sockaddr *to, int tolen)
{
  unsigned long args[6];
  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  args[4] = (unsigned long) to;
  args[5] = tolen;
  return (socketcall (SYS_SENDTO, args));
}
