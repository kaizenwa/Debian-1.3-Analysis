#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

#ifdef __SVR4_I386_ABI_L1__
#define socketcall	__socketcall
#else
static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);
#endif

int
accept(int sockfd, const struct sockaddr *peer, int *paddrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)peer;
	args[2] = (unsigned long)paddrlen;
	return socketcall(SYS_ACCEPT, args);
}

#ifdef __ELF__
#ifdef __GNUC__
int accept ( int, const struct sockaddr *, int * ) __attribute__  ((weak));
#else
#pragma weak accept
#endif   /* __GNUC__ */
#endif   /* __ELF__ */
