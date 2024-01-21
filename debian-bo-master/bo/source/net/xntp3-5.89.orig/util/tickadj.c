/*
 * tickadj - read, and possibly modify, the kernel `tick' and
 *	     `tickadj' variables, as well as `dosynctodr'.  Note that
 *	     this operates on the running kernel only.  I'd like to be
 *	     able to read and write the binary as well, but haven't
 *	     mastered this yet.
 *
 * HMS: The #includes here are different from those in xntpd/ntp_unixclock.c
 *      These seem "worse".
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include "l_stdlib.h"

#ifdef HAVE___ADJTIMEX		/* Linux */
#include <sys/timex.h>

struct timex txc;

int
main(int argc, char ** argv)
{
  if (argc > 2)
    {
      fprintf(stderr, "Usage: %s [tick_value]\n", argv[0]);
      exit(-1);
    }
  else if (argc == 2)
    {
      if ( (txc.tick = atoi(argv[1])) < 1 )
	{
	  fprintf(stderr, "Silly value for tick: %s\n", argv[1]);
	  exit(-1);
	}
#ifdef MOD_OFFSET
      txc.modes = ADJ_TICK;
#else
      txc.mode = ADJ_TICK;
#endif
    }
  else
    {
#ifdef MOD_OFFSET
      txc.modes = 0;
#else
      txc.mode = 0;
#endif
    }
    
  if (__adjtimex(&txc) < 0)
    {
      perror("adjtimex");
    }
  else
    {
      printf("tick = %ld\n", txc.tick);
    }

  return(0);
}
#else /* not Linux... kmem tweaking: */

#ifdef HAVE_SYS_FILE_H
# include <sys/file.h>
#endif
#include <sys/stat.h>

#ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif

#ifdef NLIST_STRUCT
# include <nlist.h>
#else /* not NLIST_STRUCT */ /* was defined(SYS_AUX3) || defined(SYS_AUX2) */
# include <sys/time.h>
# include <sys/resource.h>
# include <sys/file.h>
# include <a.out.h>
# include <sys/var.h>
#endif

#include "ntp_io.h"
#include "ntp_stdlib.h"

#ifdef hz /* Was: RS6000 */
# undef hz
#endif /* hz */

#ifdef HAVE_LIBKVM
# include <kvm.h>
#endif

#ifndef L_SET	/* Was: defined(SYS_PTX) || defined(SYS_IX86OSF1) */
# define L_SET SEEK_SET
#endif

#ifndef HZ
# define HZ	DEFAULT_HZ
#endif

#define	KMEM	"/dev/kmem"
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

char *progname;
int debug;

int dokmem = 1;
int writetickadj = 0;
int writeopttickadj = 0;
int unsetdosync = 0;
int writetick = 0;
int quiet = 0;
int setnoprintf = 0;

char *kmem = KMEM;
char *file = NULL;
int   fd  = -1;

static	void	getoffsets	P((unsigned long *, unsigned long *, unsigned long *, unsigned long *));
static	int	openfile	P((char *, int));
static	void	writevar	P((int, unsigned long, int));
static	void	readvar		P((int, unsigned long, int *));

/*
 * main - parse arguments and handle options
 */
void
main(argc, argv)
     int argc;
     char *argv[];
{
  int c;
  int errflg = 0;
  extern int ntp_optind;
  extern char *ntp_optarg;
  unsigned long tickadj_offset;
  unsigned long tick_offset;
  unsigned long dosync_offset;
  unsigned long noprintf_offset;
  int tickadj, ktickadj;	/* HMS: Why isn't this u_long? */
  int tick, ktick;		/* HMS: Why isn't this u_long? */
  int dosynctodr;
  int noprintf;
  int hz;
  int hz_int, hz_hundredths;
  int recommend_tickadj;
  long tmp;

  progname = argv[0];
  while ((c = ntp_getopt(argc, argv, "a:Adkqpst:")) != EOF)
    {
      switch (c)
	{
	case 'd':
	  ++debug;
	  break;
	case 'k':
	  dokmem = 1;
	  break;
	case 'p':
	  setnoprintf = 1;
	  break;
	case 'q':
	  quiet = 1;
	  break;
	case 'a':
	  writetickadj = atoi(ntp_optarg);
	  if (writetickadj <= 0)
	    {
	      (void) fprintf(stderr,
			     "%s: unlikely value for tickadj: %s\n",
			     progname, ntp_optarg);
	      errflg++;
	    }
	  break;
	case 'A':
	  writeopttickadj = 1;
	  break;
	case 's':
	  unsetdosync = 1;
	  break;
	case 't':
	  writetick = atoi(ntp_optarg);
	  if (writetick <= 0)
	    {
	      (void) fprintf(stderr,
			     "%s: unlikely value for tick: %s\n",
			     progname, ntp_optarg);
	      errflg++;
	    }
	  break;
	default:
	  errflg++;
	  break;
	}
    }
  if (errflg || ntp_optind != argc)
    {
      (void) fprintf(stderr,
		     "usage: %s [-Aqsp] [-a newadj] [-t newtick]\n", progname);
      exit(2);
    }

  getoffsets(&tick_offset, &tickadj_offset, &dosync_offset, &noprintf_offset);

  if (debug)
    {
      (void) printf("tick offset = %lu\n", tick_offset);
      (void) printf("tickadj offset = %lu\n", tickadj_offset);
      (void) printf("dosynctodr offset = %lu\n", dosync_offset);
      (void) printf("noprintf offset = %lu\n", noprintf_offset);
    }

  if (writetick && (tick_offset == 0))
    {
      (void) fprintf(stderr, 
		     "No tick kernel variable\n");
      errflg++;
    }
	
  if (writeopttickadj && (tickadj_offset == 0))
    {
      (void) fprintf(stderr, 
		     "No tickadj kernel variable\n");
      errflg++;
    }

  if (unsetdosync && (dosync_offset == 0))
    {
      (void) fprintf(stderr, 
		     "No dosynctodr kernel variable\n");
      errflg++;
    }
	
  if (setnoprintf && (noprintf_offset == 0))
    {
      (void) fprintf(stderr, 
		     "No noprintf kernel variable\n");
      errflg++;
    }

  if (tick_offset != 0)
    {
      readvar(fd, tick_offset, &tick);
#ifdef TICK_NANO
      tick /= 1000;
#endif
    }
  else
    {
      tick = 0;
    }

  if (tickadj_offset != 0)
    {
      readvar(fd, tickadj_offset, &tickadj);
#ifdef TICKADJ_NANO
      tickadj += 999;
      tickadj /= 1000;
#endif
    }
  else
    {
      tickadj = 0;
    }

  if (dosync_offset != 0)
    {
      readvar(fd, dosync_offset, &dosynctodr);
    }

  if (noprintf_offset != 0)
    {
      readvar(fd, noprintf_offset, &noprintf);
    }

  (void) close(fd);

  if (unsetdosync && dosync_offset == 0)
    {
      (void) fprintf(stderr,
		     "%s: can't find %s in namelist\n",
		     progname,
#ifdef K_DOSYNCTODR_NAME
		     K_DOSYNCTODR_NAME
#else /* not K_DOSYNCTODR_NAME */
		     "dosynctodr"
#endif /* not K_DOSYNCTODR_NAME */
		     );
      exit(1);
    }

  hz = HZ;
#if defined(HAVE_SYSCONF) && defined(_SC_CLK_TCK)
  hz = (int) sysconf (_SC_CLK_TCK);
#endif /* not HAVE_SYSCONF && _SC_CLK_TCK */
  ktick = tick;
#ifdef PRESET_TICK
  tick = PRESET_TICK;
#endif /* PRESET_TICK */
  ktickadj = tickadj;
#ifdef PRESET_TICKADJ
  tickadj = PRESET_TICKADJ;
# ifdef TICKADJ_NANO
  tickadj += 999;
  tickadj /= 1000;
# endif
#endif /* PRESET_TICKADJ */

  if (!quiet)
    {
      if (tick_offset != 0)
	{
	  (void) printf("KERNEL tick = %d us\n", ktick);
	}
#ifdef PRESET_TICK
      (void) printf("PRESET tick = %d us\n", tick);
#endif /* PRESET_TICK */
      if (tickadj_offset != 0)
	{
	  (void) printf("KERNEL tickadj = %d us\n", ktickadj);
	}
#ifdef PRESET_TICKADJ
      (void) printf("PRESET tickadj = %d us\n", tickadj);
#endif /* PRESET_TICKADJ */
      if (dosync_offset != 0)
	{
	  (void) printf("dosynctodr is %s\n", dosynctodr ? "on" : "off");
	}
      if (noprintf_offset != 0)
	{
	  (void) printf("kernel level printf's: %s\n",
			noprintf ? "off" : "on");
	}
    }

  if (tick <= 0)
    {
      (void) fprintf(stderr, "%s: the value of tick is silly!\n",
		     progname);
      exit(1);
    }

  hz_int = (int)(1000000L / (long)tick);
  hz_hundredths = (int)((100000000L / (long)tick) - ((long)hz_int * 100L));
  if (!quiet)
    {
      (void) printf("calculated hz = %d.%02d Hz\n", hz_int,
		    hz_hundredths);
    }
  tmp = (long) tick * 500L;
  recommend_tickadj = (int)(tmp / 1000000L);
  if (tmp % 1000000L > 0)
    {
      recommend_tickadj++;
    }

#ifdef MIN_REC_TICKADJ
  if (recommend_tickadj < MIN_REC_TICKADJ)
    {
      recommend_tickadj = MIN_REC_TICKADJ;
    }
#endif

  if ((!quiet) && (tickadj_offset != 0))
    {
      (void) printf("recommended value of tickadj = %d us\n",
		    recommend_tickadj);
    }

  if (   writetickadj == 0
      && !writeopttickadj
      && !unsetdosync
      && writetick == 0
      && !setnoprintf)
    {
      exit(errflg ? 1 : 0);
    }

  if (writetickadj == 0 && writeopttickadj)
    {
      writetickadj = recommend_tickadj;
    }

  fd = openfile(file, O_WRONLY);

  if (setnoprintf && (dosync_offset != 0))
    {
      if (!quiet)
	{
	  (void) fprintf(stderr, "setting noprintf: ");
	  (void) fflush(stderr);
	}
      writevar(fd, noprintf_offset, 1);
      if (!quiet)
	{
	  (void) fprintf(stderr, "done!\n");
	}
    }

  if ((writetick > 0) && (tick_offset != 0))
    {
      if (!quiet)
	{
	  (void) fprintf(stderr, "writing tick, value %d: ",
			 writetick);
	  (void) fflush(stderr);
	}
      writevar(fd, tick_offset, writetick);
      if (!quiet)
	{
	  (void) fprintf(stderr, "done!\n");
	}
    }

  if ((writetickadj > 0) && (tickadj_offset != 0))
    {
      if (!quiet)
	{
	  (void) fprintf(stderr, "writing tickadj, value %d: ",
			 writetickadj);
	  (void) fflush(stderr);
	}
      writevar(fd, tickadj_offset, writetickadj);
      if (!quiet)
	{
	  (void) fprintf(stderr, "done!\n");
	}
    }

  if (unsetdosync && (dosync_offset != 0))
    {
      if (!quiet)
	{
	  (void) fprintf(stderr, "zeroing dosynctodr: ");
	  (void) fflush(stderr);
	}
      writevar(fd, dosync_offset, 0);
      if (!quiet)
	{
	  (void) fprintf(stderr, "done!\n");
	}
    }
  (void) close(fd);
  exit(errflg ? 1 : 0);
}

/*
 * getoffsets - read the magic offsets from the specified file
 */
static void
getoffsets(tick_off, tickadj_off, dosync_off, noprintf_off)
     unsigned long *tick_off;
     unsigned long *tickadj_off;
     unsigned long *dosync_off;
     unsigned long *noprintf_off;
{
  char **kname;

#ifndef NOKMEM
# ifdef NLIST_NAME_UNION
#  define NL_B {{
#  define NL_E }}
# else
#  define NL_B {
#  define NL_E }
# endif
#endif

#define K_FILLER_NAME "DavidLetterman"

#ifdef NLIST_EXTRA_INDIRECTION
  int i;
#endif

#ifndef NOKMEM
  static struct nlist nl[] =
  {
    NL_B
#ifdef K_TICKADJ_NAME
#define N_TICKADJ	0
    K_TICKADJ_NAME
#else
    K_FILLER_NAME
#endif
    NL_E,
    NL_B
#ifdef K_TICK_NAME
#define N_TICK		1
    K_TICK_NAME
#else
    K_FILLER_NAME
#endif
    NL_E,
    NL_B
#ifdef K_DOSYNCTODR_NAME
#define N_DOSYNC	2
    K_DOSYNCTODR_NAME
#else
    K_FILLER_NAME
#endif
    NL_E,
    NL_B
#ifdef K_NOPRINTF_NAME
#define N_NOPRINTF	3
    K_NOPRINTF_NAME
#else
    K_FILLER_NAME
#endif
    NL_E,
    NL_B "" NL_E,
  };

  static char *kernels[] =
  {
#ifdef HAVE_GETBOOTFILE
    NULL,			/* *** SEE BELOW! *** */
#endif
    "/kernel/unix",
    "/kernel",
    "/vmunix",
    "/unix",
    "/mach",
    "hp-ux",
    "/386bsd",
    "/netbsd",
    "/stand/vmunix",
    "/bsd",
    NULL
  };

#ifdef HAVE_KVM_OPEN
  /*
   * Solaris > 2.5 doesn't have a kernel file.  Use the kvm_* interface
   * to read the kernel name list. -- stolcke 3/4/96
   */
  kvm_t *kvm_handle = kvm_open(NULL, NULL, NULL, O_RDONLY, progname);

  if (kvm_handle == NULL)
    {
      (void) fprintf(stderr,
		     "%s: kvm_open failed\n",
		     progname);
      exit(1);
    }
  if (kvm_nlist(kvm_handle, nl) == -1)
    {
      (void) fprintf(stderr,
		     "%s: kvm_nlist failed\n",
		     progname);
      exit(1);
    }
  kvm_close(kvm_handle);
  kname = &kernels[0];	/* dummy */
#else /* not HAVE_KVM_OPEN */
#ifdef HAVE_GETBOOTFILE		/* *** SEE HERE! *** */
  if (kernels[0] == NULL)
    {
      char * cp = (char *)getbootfile();

      if (cp)
	{
	  kernels[0] = cp;
	}
    else
      {
	kernels[0] = "/Placeholder";
      }
    }
#endif /* HAVE_GETBOOTFILE */
  for (kname = kernels; *kname != NULL; kname++)
    {
      struct stat stbuf;

      if (stat(*kname, &stbuf) == -1)
	{
	  continue;
	}
      if (nlist(*kname, nl) >= 0)
	{
	  break;
	}
      else
	{
	  (void) fprintf(stderr,
			 "%s: nlist didn't find needed symbols from <%s>: %m\n",
			 progname, *kname);
	}
    }
  if (*kname == NULL)
    {
      (void) fprintf(stderr,
		     "%s: Couldn't find the kernel\n",
		     progname);
      exit(1);
    }
#endif /* HAVE_KVM_OPEN */

  if (dokmem)
    {
      file = kmem;

      fd = openfile(file, O_RDONLY);
#ifdef NLIST_EXTRA_INDIRECTION
      /*
       * Go one more round of indirection.
       */
      for (i = 0; i < (sizeof(nl) / sizeof(struct nlist)); i++)
	{
	  if ((nl[i].n_value) && (nl[i].n_sclass == 0x6b))
	    {
	      readvar(fd, nl[i].n_value, &nl[i].n_value);
	    }
	}
#endif /* NLIST_EXTRA_INDIRECTION */
    }
#endif /* not NOKMEM */

  *tickadj_off  = 0;
  *tick_off     = 0;
  *dosync_off   = 0;
  *noprintf_off = 0;

#if defined(N_TICKADJ)
  *tickadj_off = nl[N_TICKADJ].n_value;
#endif

#if defined(N_TICK)
  *tick_off = nl[N_TICK].n_value;
#endif

#if defined(N_DOSYNC)
  *dosync_off = nl[N_DOSYNC].n_value;
#endif

#if defined(N_NOPRINTF)
  *noprintf_off = nl[N_NOPRINTF].n_value;
#endif
  return;
}

#undef N_TICKADJ
#undef N_TICK
#undef N_DOSYNC
#undef N_NOPRINTF


/*
 * openfile - open the file, check for errors
 */
static int
openfile(name, mode)
     char *name;
     int mode;
{
  int fd;

  fd = open(name, mode);
  if (fd < 0)
    {
      (void) fprintf(stderr, "%s: open %s: ", progname, name);
      perror("");
      exit(1);
    }
  return fd;
}


/*
 * writevar - write a variable into the file
 */
static void
writevar(fd, off, var)
     int fd;
     unsigned long off;
     int var;
{
	
  if (lseek(fd, off, L_SET) == -1)
    {
      (void) fprintf(stderr, "%s: lseek fails: ", progname);
      perror("");
      exit(1);
    }
  if (write(fd, (char *)&var, sizeof(int)) != sizeof(int))
    {
      (void) fprintf(stderr, "%s: write fails: ", progname);
      perror("");
      exit(1);
    }
  return;
}


/*
 * readvar - read a variable from the file
 */
static void
readvar(fd, off, var)
     int fd;
     unsigned long off;
     int *var;
{
  int i;
	
  if (lseek(fd, off, L_SET) == -1)
    {
      (void) fprintf(stderr, "%s: lseek fails: ", progname);
      perror("");
      exit(1);
    }
  i = read(fd, (char *)var, sizeof(int));
  if (i < 0)
    {
      (void) fprintf(stderr, "%s: read fails: ", progname);
      perror("");
      exit(1);
    }
  if (i != sizeof(int))
    {
      (void) fprintf(stderr, "%s: read expected %d, got %d\n",
		     progname, (int)sizeof(int), i);
      exit(1);
    }
  return;
}
#endif /* not Linux */
