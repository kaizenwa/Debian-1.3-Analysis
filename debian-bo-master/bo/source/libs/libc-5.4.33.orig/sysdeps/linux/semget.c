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
semget (key_t key, int nsems, int semflg)
{
    return __ipc (SEMGET, key, nsems, semflg, NULL);
}
