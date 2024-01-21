#ifdef __ELF__
#ifdef __GNUC__
struct sockaddr;
int getpeername( int, struct sockaddr *, int * ) __attribute__ (( weak ));
#else
#pragma weak getpeername
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
getpeername(int sockfd, struct sockaddr *addr, int *paddrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)addr;
	args[2] = (unsigned long)paddrlen;
	return socketcall(SYS_GETPEERNAME, args);
}
