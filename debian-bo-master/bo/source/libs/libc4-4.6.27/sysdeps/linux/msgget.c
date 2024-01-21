#include <syscall.h>

#define __KERNEL__ 
#include <linux/linkage.h>
#include <sys/msg.h>

int
msgget (key_t key, int msgflg)
{
    return __ipc (MSGGET, key, msgflg, 0, NULL);
}
