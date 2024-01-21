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
recvfrom (int sockfd, void *buffer, size_t len, unsigned flags,
	struct sockaddr *to, int *tolen)
{
  unsigned long args[6];
  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  args[4] = (unsigned long) to;
  args[5] = (unsigned long) tolen;
  return (socketcall (SYS_RECVFROM, args));
}

#ifdef __ELF__
#ifdef __GNUC__
int recvfrom (int, void *, size_t, unsigned, struct sockaddr *, int *)
   __attribute__ ((weak));
#else
#pragma weak recvfrom
#endif   /* __GNUC__ */
#endif   /* __ELF__ */
