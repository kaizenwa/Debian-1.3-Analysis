#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/shm.h>

char *
shmat (int shmid, char *shmaddr, int shmflg)
{
    int rval; 
    unsigned long raddr;
 
    rval = __ipc (SHMAT, shmid, shmflg, (int) &raddr, shmaddr);
    return rval < 0 ? (char *) rval : (char *) raddr;
}
