#include <syscall.h>
#include <unistd.h>

#ifdef __SVR4_I386_ABI_L1__
#define setpgid	__setpgid
#else
static inline
_syscall2(int,setpgid,pid_t,pid,pid_t,pgid);
#endif

int
setpgrp(void)
{
	return setpgid(0,0);
}
