#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/msg.h>

int
msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp, int msgflg)
{
    struct ipc_kludge tmp; 
    tmp.msgp = msgp;
    tmp.msgtyp = msgtyp; 
    return __ipc (MSGRCV, msqid, msgsz, msgflg, &tmp);
}
