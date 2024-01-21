#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/msg.h>

int
msgsnd (int msqid, struct msgbuf *msgp, int msgsz, int msgflg)
{
    return __ipc (MSGSND, msqid, msgsz, msgflg,  msgp);
}
