#include <syscall.h>
#include <linux/version.h>
#ifdef LINUX_VERSION_CODE
#if LINUX_VERSION_CODE >= 131328
#include <asm/ipc.h>
#endif
#endif

#define __KERNEL__ 
#undef asmlinkage
#define asmlinkage
#include <sys/msg.h>

int
msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp, int msgflg)
{
    struct ipc_kludge tmp; 
    tmp.msgp = msgp;
    tmp.msgtyp = msgtyp; 
    return __ipc (MSGRCV, msqid, msgsz, msgflg, &tmp);
}
