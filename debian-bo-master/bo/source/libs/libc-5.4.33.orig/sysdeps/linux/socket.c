#ifdef __ELF__
#ifdef __GNUC__
int socket(int, int, int) __attribute__ ((weak));
#else
#pragma weak socket
#endif   /* __GNUC__ */
#endif   /* __ELF__ */

#include <sys/socket.h>
#include <syscall.h>
#include <sys/socketcall.h>

#ifdef __SVR4_I386_ABI_L1__
#define socketcall __socketcall
#else
static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);
#endif

int
socket(int family, int type, int protocol)
{
	unsigned long args[3];

	args[0] = family;
	args[1] = type;
	args[2] = protocol;
	return socketcall(SYS_SOCKET, args);
}
