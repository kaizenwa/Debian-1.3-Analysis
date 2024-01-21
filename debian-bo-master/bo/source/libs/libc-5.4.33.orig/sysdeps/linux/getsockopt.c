#ifdef __ELF__
/* #pragma weak getsockopt */
int getsockopt (int, int, int, void *, int *) __attribute__ (( weak ));

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
getsockopt (int fd, int level, int optname, void *optval, int *optlen)
{
	unsigned long args[5];
	args[0]=fd;
	args[1]=level;
	args[2]=optname;
	args[3]=(unsigned long)optval;
	args[4]=(unsigned long)optlen;
	return (socketcall (SYS_GETSOCKOPT, args));
}
