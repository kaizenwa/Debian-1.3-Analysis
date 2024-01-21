#include "malloc.h"
#include <termcap.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#include <time.h>

#define ZONES 100

#define MESSAGE(t) fprintf(stdout,t)
/*#define MESSAGE(t) write(2,t,strlen(t))*/
#define PR_ALLOC() MESSAGE("Alloc... ")
#define PR_FREE()  MESSAGE("Free... ");
#define PR_FREEN() MESSAGE("Free...\n")

static char term_buffer[2048];
static char capbuffer[1024] = "";
static char *buffer = capbuffer;
char *termtype;
char *cl_string;		/* clear screen */

void
init_term ()
{
  int success;
  termtype = getenv ("TERM");
  if (termtype == (char *) 0)
    {
      fprintf (stderr, "Please, specify your terminal with 'setenv TERM <terminal>'.\n");
      exit (1);
    }
  fprintf (stderr, "Your terminal is %s\n", termtype);
  success = tgetent (term_buffer, termtype);
  if (success < 0)
    {
      fprintf (stderr, "Could not access the termcap data base.\n");
      exit (1);
    }
  if (success == 0)
    {
      fprintf (stderr, "Terminal type '%s' is not defined.\n", termtype);
      exit (1);
    }
  cl_string = tgetstr ("cl", &buffer);
  /* clear now the screen */
  printf (cl_string);
}

void
show_menu ()
{
  /* clear the screen */
  printf (cl_string);
  printf ("\t\t\tTry Checker (c) T.Gingold\n\n");
  printf ("Choose your test\n");
  printf ("\t[1] Free a block 2 times\n");
  printf ("\t[2] Change the address between malloc() and free()\n");
  printf ("\t[3] Malloc, free, and malloc with same size\n");
  printf ("\t[4] Alloc 100 kb and free\n");
  printf ("\t[5] Multiple malloc\n");
  printf ("\t[a] internal check\n");
  printf ("\t[b] free blocks\n");
  printf ("\t[c] busy blocks\n");
  printf ("\t[q] Quit\n");
  printf ("\nEnter your choice: ");
}

void
wait_key ()
{
  /* Forget the return of the sscanf */
  getchar ();
  fprintf (stderr, "Press return to continue...");
  while (getchar () != '\n')
    ;
}

void
test_alloc (int nbr)
{
  void *zones[ZONES];
  int i, n;
  static int s;
  u_int lap;

  for (i = 0; i < ZONES; i++)
    zones[i] = (void *) 0;

  lap = clock ();
  /* the test */
  for (i = 0; i < nbr; i++)
    {
      if (i % 100 == 0)
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
	  zones[n] = malloc (s);
	}
    }
  fputc ('\n', stderr);

  /* free all */
  for (i = 0; i < ZONES; i++)
    if (zones[i] != (void *) 0)
      free (zones[i]);
  printf ("It takes %ld / %d seconds\n", clock () - lap, CLK_TCK);
}

int
main (int argc, char *argv[])
{
  char *zone, *zone1, *zone2;
  char ans[20];
  int i;

  init_term ();
  srand (0x12345678);

  while (1)
    {
      show_menu ();
      scanf ("%20s", ans);
      printf (cl_string);

      for (i = 0; i < strlen (ans); i++)
	{
	  switch (tolower (ans[i]))
	    {
	    case '1':
	      MESSAGE ("Imagine you free a block 2  times...\n");
	      PR_ALLOC ();
	      zone = malloc (1000);
	      PR_FREE ();
	      free (zone);
	      PR_FREEN ();
	      free (zone);
	      break;

	    case '2':
	      MESSAGE ("Imagine you change the ptr with page_size alignment...\n");
	      PR_ALLOC ();
	      zone = malloc (1024 * 16);
	      PR_FREEN ();
	      free (zone + 1024 * 8);
	      MESSAGE ("Imagine you change the ptr...\n");
	      PR_ALLOC ();
	      zone = malloc (15);
	      PR_FREEN ();
	      free (zone + 1);
	      break;

	    case '3':
	      MESSAGE ("Imagine you alloc and free, alloc with an other size,\n");
	      MESSAGE ("and refree the first...\n");
	      PR_ALLOC ();
	      zone = malloc (1024 * 16);
	      PR_FREE ();
	      free (zone);
	      PR_ALLOC ();
	      zone1 = malloc (8 * 1024);
	      PR_FREEN ();
	      free (zone);
	      printf ("zone: 0x%x, zone1: 0x%x\n", (u_int) zone, (u_int) zone1);
	      free (zone1);
	      MESSAGE ("Imagine you alloc a fragment, free it, alloc an other with the\n");
	      MESSAGE ("same size. Just to compare the addresses.\n");
	      zone2 = malloc (64);
	      PR_ALLOC ();
	      zone = malloc (64);
	      PR_FREE ();
	      free (zone);
	      PR_ALLOC ();
	      zone1 = malloc (64);
	      PR_FREEN ();
	      free (zone1);
	      printf ("zone: 0x%x, zone1: 0x%x\n", (u_int) zone, (u_int) zone1);
	      free (zone2);
	      break;

	    case '4':
	      MESSAGE ("Alloc 100 Kb.\n");
	      PR_ALLOC ();
	      zone = malloc (100 * 1024);
#if 1
	      PR_FREEN ();
	      free (zone);
#endif
	      break;

	    case '5':
	      MESSAGE ("Test alloc 10000\n");
	      MESSAGE ("Can be rather slow\n");
	      test_alloc (10000);
	      break;

	    case '6':
	      MESSAGE ("Alloc 2 * 2048 bytes.\n");
	      PR_ALLOC ();
	      zone = malloc (2 * 1024);
	      PR_ALLOC ();
	      zone1 = malloc (2 * 1024);
	      PR_FREE ();
	      free (zone);
	      PR_FREEN ();
	      free (zone1);
	      break;

	    case '7':
	      MESSAGE ("Alloc 16kb...\n");
	      PR_ALLOC ();
	      zone = malloc (1024 * 16 - 1);
	      PR_ALLOC ();
	      zone1 = malloc (1024 * 100);
	      PR_FREEN ();
	      free (zone);
	      PR_FREEN ();
	      free (zone1);
	      break;

	    case 'a':
	      MESSAGE ("Internal check...\n");
	      __chkr_check_intern ();
	      break;

	    case 'b':
	      MESSAGE ("Free blocks:\n");
	      __chkr_dump_free ();
	      break;

	    case 'c':
	      MESSAGE ("Busy blocks:\n");
	      __chkr_dump_busy ();
	      break;

	    case 'q':
	      exit (0);

	    default:
	      fprintf (stderr, "'%c': command unknown.\n", ans[i]);
	    }
	}
      wait_key ();
    }
}
