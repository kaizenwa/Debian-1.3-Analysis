#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/msg.h>

int
msgctl (int msqid, int cmd, struct msqid_ds *buf)
{
    return __ipc (MSGCTL, msqid, cmd, 0, buf);
}
