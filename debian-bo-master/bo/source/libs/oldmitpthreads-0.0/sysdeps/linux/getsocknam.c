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
getsockname(int sockfd, struct sockaddr *addr, int *paddrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)addr;
	args[2] = (unsigned long)paddrlen;
	return socketcall(SYS_GETSOCKNAME, args);
}

#ifdef __ELF__
#ifdef __GNUC__
int getsocknam( int, struct sockaddr *, int * ) __attribute__ ((weak));
#else
#pragma weak getsockname
#endif   /* __GNUC__ */
#endif   /* __ELF__ */
