#include <sys/types.h>

extern int _chkr_brk (daddr_t);

static daddr_t curbrk;

int
chkr_brk (char *addr)
{
  daddr_t a = (daddr_t) addr;
  
  /* align.  */
  a = (a + 7) & ~7;
  
  if (_chkr_brk (a) == -1)
    return -1;

  curbrk = a;
  return a;
}

caddr_t
chkr_sbrk (int incr)
{
  daddr_t naddr;
  daddr_t oaddr = curbrk;
  
  naddr = (curbrk + incr + 7) & ~7;
  
  if (_chkr_brk (naddr) == -1)
    return -1;
  
  curbrk = naddr;
  
  return (caddr_t)oaddr;
}

extern char *end;

void
chkr_init_sbrk (void)
{
  curbrk = (daddr_t) &end;
}
