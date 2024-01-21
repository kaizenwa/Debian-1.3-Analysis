
#include "winsup.h"
#include <syslog.h>


void	closelog (void)
{
  small_printf ("Close log\n");
}

void	openlog (const char *name, int a , int b)
{
  small_printf ("Open log %s %x %x\n", name,a,b);
}
void	syslog (int b, const char *a,...)
{
  small_printf ("syslog %d %s\n", b,a);
}
