#include <syscall.h>
#include <sys/types.h>
#include <sys/wait.h>

#ifdef __SVR4_I386_ABI_L1__
#define wait4 __wait4
#else
static inline
_syscall4(__pid_t,wait4,__pid_t,pid,__WAIT_STATUS_DEFN,status,int,options,struct rusage *,ru)
#endif

__pid_t
__waitpid(__pid_t pid, int *wait_stat, int options)
{
	return wait4(pid, (__WAIT_STATUS_DEFN) wait_stat, options, NULL);
}

#include <gnu-stabs.h>
#ifdef weak_alias
weak_alias (__waitpid, waitpid);
#endif
