#ifdef __ELF__
#ifdef __GNUC__
int socketpair(int, int, int, int sockvec[2]) __attribute__ ((weak));
#else
#pragma weak socketpair
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
socketpair(int family, int type, int protocol, int sockvec[2])
{
	unsigned long args[4];

	args[0] = family;
	args[1] = type;
	args[2] = protocol;
	args[3] = (unsigned long)sockvec;
	return socketcall(SYS_SOCKETPAIR, args);
}
