#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/shm.h>

int
shmget (key_t key, int size, int shmflg)
{
    return __ipc (SHMGET, key, size, shmflg, NULL);
}
