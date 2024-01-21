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
connect(int sockfd, const struct sockaddr *saddr, int addrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)saddr;
	args[2] = addrlen;
	return socketcall(SYS_CONNECT, args);
}

#ifdef __ELF__
#ifdef __GNUC__
int connect ( int, const struct sockaddr *, int ) __attribute__ (( weak ));
#else
#pragma weak connect
#endif   /* __GNUC__ */
#endif   /* __ELF__ */
