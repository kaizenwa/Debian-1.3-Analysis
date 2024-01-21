#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/sem.h>

int
semctl (int semid, int semnum, int cmd, union semun arg)
{
    return __ipc (SEMCTL, semid, semnum, cmd, &arg); 
}
