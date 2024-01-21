#include <syscall.h>
#include <linux/version.h>
#ifdef LINUX_VERSION_CODE
#if LINUX_VERSION_CODE >= 131328
#include <asm/ipc.h>
#endif
#endif

#define __KERNEL__ 
#undef asmlinkage
#define asmlinkage
#include <sys/sem.h>

int
semctl (int semid, int semnum, int cmd, union semun arg)
{
    return __ipc (SEMCTL, semid, semnum, cmd, &arg); 
}
