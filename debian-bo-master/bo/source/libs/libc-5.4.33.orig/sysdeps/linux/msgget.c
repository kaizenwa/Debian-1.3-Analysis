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
msgget (key_t key, int msgflg)
{
    return __ipc (MSGGET, key, msgflg, 0, NULL);
}
