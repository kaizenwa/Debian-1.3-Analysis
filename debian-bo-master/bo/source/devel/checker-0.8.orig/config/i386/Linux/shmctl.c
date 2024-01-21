#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/shm.h>

extern int chkr__ipc (int call, int first, int second, int third, void *ptr);
int chkr_shmctl (int shmid, int cmd, struct shmid_ds *buf);
			
int
chkr_shmctl (int shmid, int cmd, struct shmid_ds *buf)
{
    return chkr__ipc (SHMCTL, shmid, cmd, 0, buf);
}
