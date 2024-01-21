#ifdef __ELF__
#ifdef __GNUC__
struct sockaddr;
int bind ( int, const struct sockaddr *, int ) __attribute__ (( weak ));
#else
#pragma weak bind
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
bind(int sockfd, const struct sockaddr *myaddr, int addrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)myaddr;
	args[2] = addrlen;
	return socketcall(SYS_BIND, args);
}
