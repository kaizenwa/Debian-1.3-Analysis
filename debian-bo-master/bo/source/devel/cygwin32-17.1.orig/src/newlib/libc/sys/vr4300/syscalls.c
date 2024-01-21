#include <errno.h>
#include <_ansi.h>
#include <sys/types.h>
#include <sys/stat.h>

int *
__errno()
{
  return 0;
}

_raise()
{
}

extern char _end[];

#define K0BASE (0x80000000)
#define K1BASE (0xA0000000)

char *
sbrk (nbytes)
     int nbytes;
{
  static char *heap_ptr = _end;
  char        *base;
  struct s_mem {
    unsigned int size;
    unsigned int icsize;
    unsigned int dcsize;
  } mem;
  unsigned int avail;

  get_mem_info(&mem);
  /* NOTE: The value returned from the get_mem_info call is the amount
     of memory, and not the address of the (last byte + 1) */

  if (((unsigned int)heap_ptr >= K1BASE) && ((unsigned int)heap_ptr < (K1BASE + mem.size))) {
    avail = (K1BASE + mem.size) - (unsigned int)heap_ptr;
    base = heap_ptr;
  } /* else will fail since "nbytes" will be greater than zeroed "avail" value */

  if ((nbytes > avail) || (heap_ptr + nbytes < _end))
   base = (char *)-1;
  else
   heap_ptr += nbytes;

  return base;
}


int
lseek (int file,
       int ptr,
       int dir)
{
}

int
fstat (int file,
       struct stat *st)
{
  st->st_mode = S_IFCHR;
  return 0;
}

isatty (fd)
     int fd;
{
  return 1;
}

kill(n, m)
{
  return 1;
}

getpid(n)
{
  return 1;
}
