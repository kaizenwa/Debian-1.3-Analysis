
/* Written by Peter Ekberg, peda@lysator.liu.se */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/time.h>

void
usleep(unsigned long usec)
{
  struct timeval tv;

  tv.tv_sec  = usec / 1000000L;
  tv.tv_usec = usec % 1000000L;
  select(0, NULL, NULL, NULL, &tv);
}
