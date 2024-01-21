#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/sem.h>

int
semget (key_t key, int nsems, int semflg)
{
    return __ipc (SEMGET, key, nsems, semflg, NULL);
}
