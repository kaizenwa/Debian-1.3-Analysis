#include <syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

static inline
_syscall4(__pid_t,wait4,__pid_t,pid,__WAIT_STATUS_DEFN,status,int,options,struct rusage *,ru)

__pid_t
__wait3(__WAIT_STATUS_DEFN wait_stat, int options, struct rusage *reserved)
{
	return wait4(WAIT_ANY, wait_stat, options, reserved);
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__wait3, wait3);
#endif
