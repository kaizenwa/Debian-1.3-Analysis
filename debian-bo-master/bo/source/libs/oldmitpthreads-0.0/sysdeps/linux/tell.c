#include <unistd.h>
#include <syscall.h>

#ifdef __SVR4_I386_ABI_L1__
#define lseek	__lseek
#else
static inline
_syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)
#endif

off_t tell(int);

off_t
tell (int fildes)
{
  return __lseek (fildes, 0, SEEK_CUR);
}
