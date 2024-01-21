/*
    SYSDEP.C
    Implement operating system-dependent functions required by Yorick.

    $Id: sysdep.c,v 1.1 1993/08/27 18:32:09 munro Exp munro $
 */
/*    Copyright (c) 1994.  The Regents of the University of California.
                    All rights reserved.  */

#include "ydata.h"
#include "defstr.h"
#include "defmem.h"

/*--------------------------------------------------------------------------*/
/* **************************** SIGNAL HANDLING *************************** */

#include <signal.h>

extern void SetSignals(int flags);  /* Yorick linkage */
static void HandleSignals(int sig);
static void InitializeSignals(void);
extern int y_catch_category;

#ifdef __AIX
#ifndef _AIX
#define _AIX
#endif
#endif
#ifdef _AIX
#define SAY_IT_AGAIN
#endif
#ifdef linux
#define SAY_IT_AGAIN
#endif

static void HandleSignals(int sig)
{
  signal(sig, &HandleSignals);
#ifdef SAY_IT_AGAIN
  InitializeSignals();
#endif
  if (sig==SIGINT)
    {
      y_catch_category= 0x04;
      YError("Keyboard interrupt received (SIGINT)");
    }
  else if (sig==SIGFPE)
    {
      y_catch_category= 0x01;
      YError("Floating point interrupt (SIGFPE)");
    }
  else if (sig==SIGSEGV)
    YError("Segmentation violation interrupt (SIGSEGV)");
  else if (sig==SIGILL)
    YError("Illegal instruction interrupt (SIGILL)");
#ifdef SIGBUS
  else if (sig==SIGBUS)
    YError("Misaligned address interrupt (SIGBUS)");
#endif
  YError("Unrecognized signal delivered to HandleSignals");
}

static int setSignalsCalled= 0;

void SetSignals(int flags)
{
  if (!setSignalsCalled) InitializeSignals();
  signal(SIGINT, flags&1? &HandleSignals : SIG_DFL);
  signal(SIGFPE, flags&2? &HandleSignals : SIG_DFL);
  signal(SIGSEGV, flags&4? &HandleSignals : SIG_DFL);
  signal(SIGILL, flags&8? &HandleSignals : SIG_DFL);
#ifdef SIGBUS
  signal(SIGBUS, flags&16? &HandleSignals : SIG_DFL);
#endif
}

/* This routine is for fascist operating systems which insist that each
   program must individually know about the brain dead IEEE 754 standard
   which decrees that SIGFPE will never be generated.  On more reasonable
   machines, there will be a loader switch to start programs in the mode
   in which non-nuisance exceptions cause SIGFPE to be raised.  If no
   such switch exists, a branch must be added to InitializeSignals, which
   is called once when Yorick starts.  */
static void InitializeSignals(void)
{
#if 0
#ifdef sgi
  /* References:   sys/fpu.h header file (defines bits used below)
                   fpc, math, sigfpe man pages   */
#include <sys/fpu.h>
  int csr= get_fpc_csr();   /*  returns 0x00800004  */
  set_fpc_csr(csr | 0x0103c000);  /* core dumps */
#endif
#endif
#ifdef sgi
  /* See man page on handle_sigfpes -- must link with -lfpe */
#include <sigfpe.h>
  handle_sigfpes(_ON, _EN_OVERFL|_EN_DIVZERO|_EN_INVALID,
		 (void (*)())0, _REPLACE_HANDLER_ON_ERROR, &HandleSignals);
#endif
#ifdef sun
  /* References:   ieee_handler man page (SunOS or Solaris)
                   on Solaris, you may need -lsunmath
		   nonstandard_arithmetic is undocumented, but rumored
		   to be important to get rapid underflows
     Sun's -fnonstd compiler switch buggers crt1.o under SunOS, and
     switches between __fnonstd.o and __fstd.o under Solaris, as far
     as I can tell.
     This is a bit of a problem when compiling with gcc...  */
  nonstandard_arithmetic();
  ieee_handler("set","common", &HandleSignals);
#endif
#ifdef __hppa
#ifndef hppa
#define hppa
#endif
#endif
#ifdef hppa
  /* References:   fpsetmask man page */
#include <math.h>
  fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL);  /* 0x1c */
  fpsetfastmode(1);    /* fast underflows */
#endif
#ifdef _AIX
  /* References:   fp_trap, fp_enable man pages */
#include <fptrap.h>
  fp_trap(FP_TRAP_FASTMODE);
  fp_enable(TRP_INVALID | TRP_DIV_BY_ZERO | TRP_OVERFLOW);
#endif
#ifdef __alpha
  /* References:   exception_intro, ieee man pages */
#include <machine/fpu.h>
  ieee_set_fp_control(IEEE_TRAP_ENABLE_INV | IEEE_TRAP_ENABLE_DZE |
		      IEEE_TRAP_ENABLE_OVF);
#endif
#ifdef cray
  /* UNICOS delivers SIGFPE by default, but no matherr */
#ifdef HAS_LIBMSET
  { int flag=-1 ; libmset(&flag); }
#endif
#endif
#ifdef linux
  /* Linux used to deliver SIGFPE by default, but no longer.  Sigh.  */
  __setfpucw(0x1372);
#endif
#if defined(NeXT) && defined(m68k) && defined(__GNUC__)
  /* NeXT needs explicit csr set to generate SIGFPE */
  asm("fmovel     #0x1400,fpcr");   /* set OVFL and ZD bits */
#endif
  setSignalsCalled= 1;
}

/*--------------------------------------------------------------------------*/
/* ************************* ENVIRONMENT VARIABLES ************************ */

#include "yio.h"

/* getenv should be declared in <stdlib.h> */
extern char *getenv(const char *);

/* Use StrFree to get rid of return value from Ygetenv (wraps getenv).  */
char *Ygetenv(const char *name)
{
  return StrCpy(getenv(name));
}

/*--------------------------------------------------------------------------*/
/* ************************** SPECIAL DIRECTORIES ************************* */

/* declared in yio.h */

/* getcwd and chdir should be in <unistd.h>
   SunOS deprecates use of this in favor of getwd, but apparently getcwd
   is more portable...
   Comments in HPUX unistd.h indicate that original POSIX.1 1988
   definition had second parameter of type int; size_t (long) as here
   makes more sense and has apparently superceded the original.
   On machines with sizeof(int)<sizeof(long) this could be serious... */
extern char *getcwd(char *, unsigned long);
extern int chdir(const char *);
#ifndef HAS_GETCWD
/* if neither getcwd nor getwd is present, we're out of luck */
extern char *getwd(char *);
char *getcwd(char *b, unsigned long s) { return getwd(b); }
#endif

char *yCWD= 0;
char *yHOME= 0;

/* YSetCWD returns non-0 if operation fails, resets yCWD on success.  */
int YSetCWD(const char *name)
{
  int notOK= name? chdir(name) : 0;
  if (!notOK) {
    /* This version makes no attempt to deal with directory names
       longer than 1024 characters (system will probably have trouble
       with longer pathnames anyway).  */
    char *tmp= StrAlloc(1025);       /* SunOS wants 2 extra characters... */
    StrFree(yCWD);
    yCWD= StrCpy(getcwd(tmp, 1024)); /* Note that this may be 1024L... */
    YNameToHead(&yCWD);
    StrFree(tmp);
  }
  return notOK;
}

void YGetHOME(void)
{
  yHOME= Ygetenv("HOME");
  YNameToHead(&yHOME);
}

/*--------------------------------------------------------------------------*/
/* **************************** DIRECTORY PATHS *************************** */

/* declared in yio.h */

char *YPATHstrtok(char *paths)
{
  return strtok(paths, PATH_SEP_DELIMIT);
}

/*--------------------------------------------------------------------------*/
/* *************************** FILENAME HANDLING ************************** */

/* declared in yio.h */

#ifndef NO_GETPWNAM
/* May need <sys/types.h> as well according to POSIX standard?  */
#include <pwd.h>
#endif

/* Convert path name which may be relative, or begin with ., .., ~, or
   an environment variable, into an absolute pathname.  */
char *YExpandName(const char *name)
{
  char *path, *head= 0, *tail0= 0;
  char *tail= (char *)name;  /* I promise not to write to the original... */
  int freeHead= 0;
  int nRemove= 0;   /* count number of leading ..s */

  if (!name) return 0;

  /* try to take care of simple environment variable in first position */
  if (tail[0]=='$') {
    char *env;
    char delim= *(++tail);
    if (delim=='(') { delim= ')'; tail++; }
    else if (delim=='{') { delim= '}'; tail++; }
    else delim= DIR_SEP;
    env= tail;
    while (*tail && *tail!=delim) tail++;
    if (*tail && delim!=DIR_SEP) tail++;
    head= Ygetenv(env= StrNCpy(env, tail-env));
    StrFree(env);
    tail= tail0= StrCat(head, tail);
    StrFree(head);
    head= 0;
  }

  /* handle paths beginning with . or ~ or relative pathnames */
  if (tail[0]=='.' || tail[0]=='~') {
    if (tail[0]=='.')                      head= yCWD;
#ifndef NO_GETPWNAM
    else if (!tail[1] || tail[1]==DIR_SEP) head= yHOME;
    else {
      struct passwd *pw;
      char sep;
      if (!tail0) tail= tail0= StrCpy(tail);
      while (tail[1] && tail[1]!=DIR_SEP) tail++;
      sep= tail[1];
      tail[1]= '\0';   /* be sure user name is 0 terminated */
      pw= getpwnam(tail0+1);
      tail[1]= sep;    /* restore original tail */
      if (pw) {
	head= StrCpy(pw->pw_dir);
	freeHead= 1;
	YNameToHead(&head);
      } else {
	tail= tail0;
      }
    }
#else
    /* without getpwnam function, can't handle ~user/dir1/dir2/... */
    else head= yHOME;
#endif
    if (!tail[1]) tail++;
    else if (tail[1]==DIR_SEP) tail+= 2;
  } else if (tail[0]!=DIR_SEP) {
    head= yCWD;
  }

  /* count number of leading ..s */
  while (tail[0]=='.') {
    if (tail[1]=='.') {
      if (!tail[2]) {
	tail+= 2;
	nRemove++;
      } else if (tail[2]==DIR_SEP) {
	tail+= 3;
	nRemove++;
      }
    } else if (tail[1]==DIR_SEP) {
      tail+= 2;
    } else if (!tail[1]) {
      tail++;
    } else {
      break;
    }
  }

  /* strip nRemove parent directories (but stop at root) */
  if (nRemove && head) {
    char *old= head;
    head= StrCpy(head);
    if (freeHead) StrFree(old);
    else freeHead= 1;
    path= head+strlen(head)-1;   /* guaranteed to point to DIR_SEP */
    do {
      while (path>head && *(--path)!=DIR_SEP);
    } while (--nRemove);
    path[1]= '\0';
  }

  path= StrCat(head, tail);
  if (freeHead) StrFree(head);
  if (tail0) StrFree(tail0);
  return path;
}

/* Strip leading directory names from a pathname.  */
char *YNameTail(const char *name)
{
  const char *nm= name;
  if (!nm) return 0;
  nm+= strlen(nm);
  while (nm>name && *nm!=DIR_SEP) nm--;
  if (*nm!=DIR_SEP) return StrCpy(nm);
  else return StrCpy(nm+1);
}

/* Return leading directory names of a pathname, including trailing /.
   Returns 0 if no occurrences of / in pathname.  */
char *YNameHead(const char *name)
{
  const char *nm= name;
  if (!nm) return 0;
  nm+= strlen(nm);
  while (nm>name && *nm!=DIR_SEP) nm--;
  if (*nm!=DIR_SEP) return 0;
  else return StrNCpy(name, nm-name+1);
}

/* Ensure that a name ends in /, so it can be used as a path prefix.  */
void YNameToHead(char **name)
{
  char *head= *name;
  long n= head? strlen(head) : 0;
  if (n<1 || head[n-1]!=DIR_SEP) {
    head= StrCat(head, DIR_SEP_S);
    StrFree(*name);
    *name= head;
  }
}

int YIsAbsolute(const char *name)
{
#ifndef IBM_PC_DOS
  return name[0]==DIR_SEP;
#else
  /* must also check for drive/device name */
  return name[0]==DIR_SEP || HasDrivePrefix(name);
#endif
}

int YIsDotRelative(const char *name)
{
  return name[0]=='.' &&
    (!name[1] || (name[1]==DIR_SEP ||
		  (name[1]=='.' && (!name[2] || name[2]==DIR_SEP))));
}

/*--------------------------------------------------------------------------*/
/* ****************************** CODE TIMING ***************************** */

/* Yorick linkage */
extern char *Ytimestamp(void);  /* 24 character return */
extern void Ytimer(double *cpu, double *sys, double *wall);

static double initWall= -1.0e31;

#ifdef NO_TIMER
#ifndef CLOCK_FUNCTION
#define CLOCK_FUNCTION clock
#endif
#endif

#ifndef CLOCK_FUNCTION
#ifdef BSD_TIMER
/* This will work on our Suns -- other BSD machines may differ?
    sys/time.h includes time.h on Sun */
#include <sys/time.h>
#include <sys/resource.h>
void Ytimer(double *cpu, double *sys, double *wall)
{
  struct rusage cpuTime;
  struct timeval wallTime;
  struct timezone zone;
  getrusage(RUSAGE_SELF, &cpuTime);
  *cpu= cpuTime.ru_utime.tv_sec + 1.0e-6*cpuTime.ru_utime.tv_usec;
  *sys= cpuTime.ru_stime.tv_sec + 1.0e-6*cpuTime.ru_stime.tv_usec;
  gettimeofday(&wallTime, &zone);
  *wall= wallTime.tv_sec + 1.0e-6*wallTime.tv_usec;
  if (initWall<-1.0e30) initWall= *wall;
  *wall-= initWall;
}

#else
/* Assume POSIX 1003.1-1990 standard timing interface.
   However-- CLK_TCK is noted as "obsolescent" there...  */
#include <time.h>
#include <sys/times.h>
/* Try to handle modest deviations from POSIX standard (e.g.- Sun).  */
#ifndef CLK_TCK
#include <unistd.h>
#ifndef CLK_TCK
#define CLK_TCK sysconf(_SC_CLK_TCK)
#endif
#endif
static double ticksPerSecond= 0.0;
void Ytimer(double *cpu, double *sys, double *wall)
{
  struct tms cpuTime;
  long wallTicks= times(&cpuTime);
  if (ticksPerSecond==0.0) ticksPerSecond= CLK_TCK;
  *cpu= cpuTime.tms_utime/(double)ticksPerSecond;
  *sys= cpuTime.tms_stime/(double)ticksPerSecond;
  *wall= wallTicks/(double)ticksPerSecond;
  if (initWall<-1.0e30) initWall= *wall;
  *wall-= initWall;
}

#endif
#else
/* Define CLOCK_FUNCTION then modify the following version to suit
   your architecture.  See sysdep.h for other suggestions.
 */
#include <time.h>

void Ytimer(double *cpu, double *sys, double *wall)
{
  *cpu= *wall= ((double)CLOCK_FUNCTION()) / ((double)TICKS_PER_SEC);
  *sys= 0.0;
  if (initWall<-1.0e30) initWall= *wall;
  *wall-= initWall;
}
#endif

/* ctime and time are from ANSI <time.h> -- should have been
   included above... */

char *Ytimestamp(void)
{
  time_t now= time((void *)0);
  return StrCpy(strtok(ctime(&now), "\n"));
}

/*--------------------------------------------------------------------------*/
/* *************************** NON-BLOCKING I/O *************************** */

/* YstdinNB(0) is supposed to pause until input arrives on stdin, so that
     the next call to fgets will return immediately with the new input.
   YstdinNB(1) returns immediately no matter what.
   In either case, the return value is 0 if and only if fgets will return
   immediately.  */
extern int YstdinNB(int noWait);

/* Hopefully, the true timeval struct (in sys/time.h on Suns) will
   not be any longer than twice this guess, hence the bogus two element
   array.  Its value just needs to represent zero time.  */
static struct timevalFake {
  long	tv_sec;		/* seconds */
  long	tv_usec;	/* and microseconds */
} zeroDelay[2]= { {0, 0}, {0, 0} };

/* As far as I can tell, the select function is supposed to have
   the following prototype:
   int select(size_t, int *, int *, int *, timeval *);
   However, it often doesn't.  At least once, it has been declared
   in stdio.h, so it is dangerous to give a prototype, lest it
   conflict.  The major problem would be if the mask argument were
   really a long* on a big-endian machine with sizeof(long)>sizeof(int).
   Also possible that the first argument is an int.  */

/* Prototype is in time.h on HPs -- best to leave it untyped, but
   following above includes...
   extern int select(size_t, int *, int *, int *, const struct timeval *);
 */

int YstdinNB(int noWait)
{
  void *delay= noWait? (void *)zeroDelay : (void *)0;
  int mask[2];   /* to reduce the obscurity of nasty error mentioned above */
  mask[0]= 1;    /* assume stdin has file descriptor 0 */
  return (select(1L, (void *)mask, (void *)0, (void *)0, delay) > 0);
}

/*--------------------------------------------------------------------------*/
/* *************************** LAUNCH DIRECTORY *************************** */

/* Attempt to track down the directory from which this program was
   launched by examining the string in argv[0]:
      (0) Use YORICK_LAUNCH environment variable if present.
      (1) If this is an absolute path, begin with the associated
          directory, oherwise, try to find an executable file of this
	  name on the PATH environment variable, and begin with that
	  directory.
      (2) If the file is a soft link, track down the link to find
          the directory from which the code was actually launched.
	  Also, track THAT directory to be sure it is not a soft link.
   Sets yLaunchDir and returns non-zero on success.

   Also, check for a soft link yorick.site in the launch directory
   which will be used to override the default site directory loaded
   into ySiteDir by Codger (in ycode.c).

   All of this is intended to allow Yorick binaries to be moved into
   an environment other than the one they were built in.
 */
extern int YGetLaunchDir(char *argv0);
char *yLaunchDir= 0;   /* yio.h -- ySiteDir defined in ycode.c */
static int siteDone= 0;

#ifndef NO_SOFT_LINKS
static int TrackSoftLinks(char **name);
#endif

int YGetLaunchDir(char *argv0)
{
  char *executable= Ygetenv("YORICK_LAUNCH");

  /* Set yCWD and yHOME if they haven't been initialized --
     these are necessary for names to be exapnded properly below */
  if (!yCWD) YSetCWD((char *)0);
  if (!yHOME) YGetHOME();

  if (executable && executable[0]) {
    YNameToHead(&executable);
    yLaunchDir= executable;
    return 1;
  }
  StrFree(executable);
  executable= 0;

  if (!argv0 || !argv0[0]) return 0;

  if (YIsAbsolute(argv0)) {
    executable= StrCpy(argv0);

  } else if (YIsDotRelative(argv0)) {
    executable= YExpandName(argv0);

  } else {
    char *paths= Ygetenv("PATH");
    if (paths) {
      char *name=0, *dirname=0;
      char *dir= YPATHstrtok(paths);
      for ( ; dir ; dir=YPATHstrtok((char *)0)) {
	StrFree(dirname);
	dirname= StrCpy(dir);
	YNameToHead(&dirname);
	StrFree(name);
	name= StrCat(dirname, argv0);
	/* Note: access function checks for existence of file
	         -- it is POSIX standard, defined in <unistd.h>,
		    but the 2nd argument 0 is actually F_OK there
		 GNU Emacs does it like this: */
	if (access(name,0)>=0) break;
      }
      StrFree(dirname);
      StrFree(paths);
      if (dir) executable= YExpandName(name);
      StrFree(name);
    }
  }

#ifndef NO_SOFT_LINKS
  /* executable itself may be a soft link */
  TrackSoftLinks(&executable);
#endif

  yLaunchDir= YNameHead(executable);
  StrFree(executable);

#ifndef NO_SOFT_LINKS
  /* directory containing the executable may also be a soft link */
  if (yLaunchDir) {
    long len= strlen(yLaunchDir);
    if (len>0 && yLaunchDir[len-1]==DIR_SEP) {
      yLaunchDir[len-1]= '\0';
      TrackSoftLinks(&yLaunchDir);
      YNameToHead(&yLaunchDir);
    }
  }
#endif

  /* Check for yorick.site soft link and override ySiteDir if present.
     Non-UNIX OSs may have another way to override ySiteDir.  */
  if (!siteDone) {
#ifndef NO_SOFT_LINKS
    char *altSite= StrCat(yLaunchDir,"yorick.site");
    if (TrackSoftLinks(&altSite)) {
      ySiteDir= altSite;
    } else {
      StrFree(altSite);
      ySiteDir= StrCpy(ySiteDir);
    }
#else
    ySiteDir= StrCpy(ySiteDir);
#endif
    YNameToHead(&ySiteDir);
    siteDone= 1;
  }

  return (yLaunchDir && yLaunchDir[0]);
}

#ifndef NO_SOFT_LINKS
static int TrackSoftLinks(char **name)
{
  int found= 0;
  if (*name) {
    /* track through all soft links to find actual executable file */
    char *link= StrAlloc(1025);
    int len= readlink(*name, link, 1024);
    while (len>=0) {
      found= 1;
      link[len]= '\0';
      StrFree(*name);
      *name= StrCpy(link);
      len= readlink(*name, link, 1024);
    }
    StrFree(link);
  }
  return found;
}
#endif

/*--------------------------------------------------------------------------*/
