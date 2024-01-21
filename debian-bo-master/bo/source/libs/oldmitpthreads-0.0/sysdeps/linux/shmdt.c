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
#include <sys/shm.h>

int
shmdt (char *shmaddr)
{
    return __ipc (SHMDT, 0, 0, 0, shmaddr);
}
