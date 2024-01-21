#include "malloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>

#define ZONES 100

void
test_alloc (int nbr)
{
  void *zones[ZONES];
  int i, n;
  static int s, s1;
  u_int lap;

  for (i = 0; i < ZONES; i++)
    zones[i] = (void *) 0;

  lap = clock ();
  /* the test */
  for (i = 0; i < nbr; i++)
    {
      if (i % 1 /*00*/  == 0)
	fprintf (stderr, "\r%d", i);
      __chkr_check_intern ();
      n = rand () / (RAND_MAX / ZONES);
      if (n < 0 || n >= ZONES)
	{
	  fprintf (stderr, "\bn=%d\n", n);
	  continue;
	}
      if (zones[n] != (void *) 0)
	{
	  free (zones[n]);
	  zones[n] = (void *) 0;
	}
      else
	{
	  s = rand () / (RAND_MAX / 32768);
	  s1 = rand () / (RAND_MAX / 1000) + 1;
	  zones[n] = memalign (s1, s);
	}
    }
  fputc ('\n', stderr);

  /* free all */
  for (i = 0; i < ZONES; i++)
    if (zones[i] != (void *) 0)
      free (zones[i]);
  printf ("It takes %ld / %d seconds\n", clock () - lap, CLK_TCK);
}

main ()
{
  char *zone1, *zone2;

  zone1 = memalign (100, 20);
  zone2 = valloc (2000);
  free (zone1);
  free (zone2);
  test_alloc (1000);
}
