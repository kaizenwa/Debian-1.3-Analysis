#include <syscall.h>
#include <sys/timex.h>
#include <linux/timex.h>

#ifdef __SVR4_I386_ABI_L1__
#define adjtimex __adjtimex
#else
inline static
_syscall1(int, adjtimex, struct timex *, ntx);
#endif

int
__ntp_gettime (struct ntptimeval * ntv)
{
  struct timex tntx;
  int result;

  result = adjtimex(&tntx);
  ntv->time = tntx.time;
  ntv->maxerror = tntx.maxerror;
  ntv->esterror = tntx.esterror;
  return result;
}
