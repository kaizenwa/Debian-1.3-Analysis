#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/sem.h>

int
semop (int semid, struct sembuf *sops, unsigned nsops)
{
    return __ipc (SEMOP, semid, (int) nsops, 0, sops);
}
