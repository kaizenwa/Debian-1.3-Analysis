#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/shm.h>

int
shmdt (char *shmaddr)
{
    return __ipc (SHMDT, 0, 0, 0, shmaddr);
}
