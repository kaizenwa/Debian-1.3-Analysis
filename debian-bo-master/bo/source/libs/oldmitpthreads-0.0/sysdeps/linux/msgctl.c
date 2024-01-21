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
msgctl (int msqid, int cmd, struct msqid_ds *buf)
{
    return __ipc (MSGCTL, msqid, cmd, 0, buf);
}
