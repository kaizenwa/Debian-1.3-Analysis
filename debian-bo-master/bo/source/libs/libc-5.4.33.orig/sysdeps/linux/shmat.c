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

char *
shmat (int shmid, char *shmaddr, int shmflg)
{
    int rval; 
    unsigned long raddr;
 
    rval = __ipc (SHMAT, shmid, shmflg, (int) &raddr, shmaddr);
    return rval < 0 ? (char *) rval : (char *) raddr;
}
