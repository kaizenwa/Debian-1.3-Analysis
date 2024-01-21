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
shmctl (int shmid, int cmd, struct shmid_ds *buf)
{
    return __ipc (SHMCTL, shmid, cmd, 0, buf);
}
