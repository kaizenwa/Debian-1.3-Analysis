/*
 * A swapon(8)/swapoff(8) for Linux 0.99.
 * swapon.c,v 1.1.1.1 1993/11/18 08:40:51 jrs Exp
 * Added '-s' (Summary option) <Vincent.Renardias@waw.com> 02/1997.
 */

#include "swap.h"
#include "swapargs.h"
#include "sundries.h"

#define        PROC_SWAPS      "/proc/swaps"

/* #define SWAPON_NEEDS_TWO_ARGS */

/* Nonzero for chatty (-v).  This is a nonstandard flag (not in BSD).  */
int verbose = 0;
int priority = -1;	/* non-prioritized swap by default */

extern char version[];
static char *program_name;
static struct option longopts[] =
{
  { "all", 0, 0, 'a' },
  { "help", 0, 0, 'h' },
  { "priority", required_argument, 0, 'p' },
  { "summary", 0, 0, 's' },
  { "verbose", 0, 0, 'v' },
  { "version", 0, 0, 'V' },
  { NULL, 0, 0, 0 }
};

const char *usage_string = "\
usage: %s [-hV]\n\
       %s -a [-v]\n\
       %s [-v] [-p priority] special ...\n\
       %s [-s]\n\
";

static void
usage (FILE *fp, int n)
{
  fprintf (fp, usage_string, program_name, program_name, program_name, program_name);
  exit (n);
}

#ifdef SWAPON_HAS_TWO_ARGS
#define SWAPON_NEEDS_TWO_ARGS
#endif

#ifdef SWAPON_NEEDS_TWO_ARGS
#ifdef SWAPON_HAS_TWO_ARGS
/* libc is OK */
#include <unistd.h>
#else
/* We want a swapon with two args, but have an old libc.
   Build the kernel call by hand. */
#include <linux/unistd.h>
static
_syscall2(int,  swapon,  const char *,  path, int, flags);
static
_syscall1(int,  swapoff,  const char *,  path);
#endif
#else
/* just do as libc says */
#include <unistd.h>
#endif

static int
swap (const char *special, int prio)
{
  int status;

  if (verbose)
    printf("%s on device %s\n", program_name, special);

  if (streq (program_name, "swapon")) {
#ifdef SWAPON_NEEDS_TWO_ARGS
     int flags = 0;

#ifdef SWAP_FLAG_PREFER
     if (prio >= 0) {
        if (prio > SWAP_FLAG_PRIO_MASK)
	  prio = SWAP_FLAG_PRIO_MASK;
	flags = SWAP_FLAG_PREFER
		| ((prio & SWAP_FLAG_PRIO_MASK) << SWAP_FLAG_PRIO_SHIFT);
     }
#endif
     status = swapon (special, flags);
#else
     status = swapon (special);
#endif
  } else
     status = swapoff (special);

  if (status < 0)
    fprintf (stderr, "%s: %s: %s\n", program_name, special, strerror (errno));

  return status;
}

int
display_summary(void)
{
       FILE *swaps;
       char line[200] ;

       if ((swaps = fopen(PROC_SWAPS, "r")) == NULL) {
               fprintf (stderr, "%s: %s: %s\n", program_name, PROC_SWAPS,
			strerror (errno));
               return -1 ; 
       }
       while ( fgets(line, sizeof(line), swaps))
               printf ("%s", line);

       return 0 ;
}

int
main (int argc, char *argv[])
{
  struct fstab *fstab;
  int status;
  int all = 0;
  int c;

  if (strrchr (argv[0], '/') != NULL)
    program_name = strrchr (argv[0], '/') + 1;
  else
    program_name = argv[0];

  while ((c = getopt_long (argc, argv, "ahp:svV", longopts, NULL)) != EOF)
    switch (c)
      {
      case 'a':			/* all */
	++all;
	break;
      case 'h':			/* help */
	usage (stdout, 0);
	break;
      case 'p':			/* priority */
	priority = atoi(optarg);
	break;
      case 's':			/* tell about current use of swap areas */
	status = display_summary();
	exit(status);
      case 'v':			/* be chatty */
	++verbose;
	break;
      case 'V':			/* version */
	printf ("swapon: %s\n", version);
	exit (0);
      case 0:
	break;
      case '?':
      default:
	usage (stderr, 1);
      }

  argv += optind;

  status = 0;

  if (all)
    {
      while ((fstab = getfsent()) != NULL)
	if (streq (fstab->fs_type, FSTAB_SW))
	{
	   /* parse mount options; */
	   char *opt, *opts = strdup(fstab->fs_mntopts);
	   
	   for (opt = strtok (opts, ",");
		opt != NULL;
		opt = strtok (NULL, ","))
	   {
	      if (strncmp(opt, "pri=", 4) == 0)
	      {
		 priority = atoi(opt+4);
	      }
	   }
	   status |= swap (fstab->fs_spec, priority);
	}
    }
  else if (*argv == NULL)
    {
      usage (stderr, 2);
    }
  else
    {
       while (*argv != NULL)
	  status |= swap (*argv++,priority);
    }
  return status;
}
