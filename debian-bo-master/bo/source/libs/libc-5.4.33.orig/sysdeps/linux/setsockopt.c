#ifdef __ELF__
#ifdef __GNUC__
int setsockopt (int, int, int, const void *, int) __attribute__ ((weak));
#else
#pragma weak setsockopt
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

/* [sg]etsockoptions by bir7@leland.stanford.edu */

int
setsockopt (int fd, int level, int optname, const void *optval,
	int optlen)
{
	unsigned long args[5];
	args[0]=fd;
	args[1]=level;
	args[2]=optname;
	args[3]=(unsigned long)optval;
	args[4]=optlen;
	return (socketcall (SYS_SETSOCKOPT, args));
}
