/* signal.c */

#include <signal.h>

_sig_func_ptr
signal (int sig, _sig_func_ptr handler)
{
  return SIG_DFL;	/* FIXME: Until this is finished.  */
}
