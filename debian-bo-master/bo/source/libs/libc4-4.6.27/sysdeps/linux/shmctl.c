#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/shm.h>

int
shmctl (int shmid, int cmd, struct shmid_ds *buf)
{
    return __ipc (SHMCTL, shmid, cmd, 0, buf);
}
