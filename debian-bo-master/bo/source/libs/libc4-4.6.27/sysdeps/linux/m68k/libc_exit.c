#include <unistd.h>
#include <sys/syscall.h>

void
_exit(int exit_code)
{
  __asm__ volatile ("moveq %0,d0;movel %1,d1;trap #0"
                    ::"i" (SYS_exit),"g" (exit_code) : "d0", "d1");
}
