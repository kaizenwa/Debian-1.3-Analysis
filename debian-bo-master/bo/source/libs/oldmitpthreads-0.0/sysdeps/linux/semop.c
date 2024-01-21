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
semop (int semid, struct sembuf *sops, unsigned nsops)
{
    return __ipc (SEMOP, semid, (int) nsops, 0, sops);
}
