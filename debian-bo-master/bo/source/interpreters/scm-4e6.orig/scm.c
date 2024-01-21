/* Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * As a special exception, the Free Software Foundation gives permission
 * for additional uses of the text contained in its release of GUILE.
 *
 * The exception is that, if you link the GUILE library with other files
 * to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the GUILE library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the
 * Free Software Foundation under the name GUILE.  If you copy
 * code from other Free Software Foundation releases into a copy of
 * GUILE, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for GUILE, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.  
 */

/* "scm.c" top level and interrupt code.
   Author: Aubrey Jaffer */

#include <signal.h>
#include "scm.h"
#include "patchlvl.h"

#ifdef __IBMC__
# include <io.h>
#endif

#ifndef STDC_HEADERS
	int alarm P((unsigned int));
	int pause P((void));
	unsigned int sleep P((unsigned int seconds));
	char *getenv P((const char *name));
	int system P((const char *));
#endif
#ifdef hpux
# define const /**/
#endif

void	final_repl P((void));
void	init_dynl P((void));
void	init_eval P((void));
void	init_features P((void));
void	init_io P((void));
void	init_ioext P((void));
void	init_repl P((int iverbose));
void	init_sc2 P((void));
void	init_scl P((void));
void	init_signals P((void));
void	init_subrs P((void));
void	init_tables P((void));
void	init_time P((void));
void	init_types P((void));
void	init_unif P((void));
void	init_ramap P((void));

void init_banner()
{
  fputs("SCM version ", stderr);
  fputs(SCMVERSION, stderr);
  fputs(", Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 \
Free Software Foundation.\n\
SCM comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.\n\
This is free software, and you are welcome to redistribute it\n\
under certain conditions; type `(terms)' for details.\n", stderr);
}

SCM scm_init_extensions()
{
#ifdef COMPILED_INITS
    COMPILED_INITS;		/* initialize statically linked add-ons */
#endif
    return UNSPECIFIED;
}

#if (__TURBOC__==1)
# define signal ssignal		/* Needed for TURBOC V1.0 */
#endif

/* SIGRETTYPE is the type that signal handlers return.  See <signal.h>*/

#ifdef RETSIGTYPE
# define SIGRETTYPE RETSIGTYPE
#else
# ifdef STDC_HEADERS
#  if (__TURBOC__==1)
#   define SIGRETTYPE int
#  else
#   define SIGRETTYPE void
#  endif
# else
#  ifdef linux
#   define SIGRETTYPE void
#  else
#   define SIGRETTYPE int
#  endif
# endif
#endif

#ifdef vms
# ifdef __GNUC__
#  define SIGRETTYPE int
# endif
#endif

#ifdef SIGHUP
static SIGRETTYPE hup_signal(sig)
     int sig;
{
	signal(SIGHUP, hup_signal);
	wta(UNDEFINED, (char *)HUP_SIGNAL, "");
}
#endif
static SIGRETTYPE int_signal(sig)
     int sig;
{
	sig = errno;
	signal(SIGINT, int_signal);
	if (ints_disabled) sig_deferred = 1;
	else han_sig();
	errno = sig;
}

/* If doesn't have SIGFPE, disable FLOATS for the rest of this file. */

#ifndef SIGFPE
# undef FLOATS
#endif

#ifdef FLOATS
static SIGRETTYPE fpe_signal(sig)
     int sig;
{
	signal(SIGFPE, fpe_signal);
	wta(UNDEFINED, (char *)FPE_SIGNAL, "");
}
#endif
#ifdef SIGBUS
static SIGRETTYPE bus_signal(sig)
     int sig;
{
	signal(SIGBUS, bus_signal);
	wta(UNDEFINED, (char *)BUS_SIGNAL, "");
}
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
static SIGRETTYPE segv_signal(sig)
     int sig;
{
	signal(SIGSEGV, segv_signal);
	wta(UNDEFINED, (char *)SEGV_SIGNAL, "");
}
#endif
#ifdef atarist
# undef SIGALRM			/* only available via MiNT libs */
#endif
#ifdef GO32
# undef SIGALRM
#endif
#ifdef __HIGHC__
# undef SIGALRM
#endif
#ifdef SIGALRM
static SIGRETTYPE alrm_signal(sig)
     int sig;
{
	sig = errno;
	signal(SIGALRM, alrm_signal);
	if (ints_disabled) alrm_deferred = 1;
	else han_alrm();
	errno = sig;
}
static char s_alarm[] = "alarm";
SCM lalarm(i)
     SCM i;
{
  unsigned int j;
  ASSERT(INUMP(i) && (INUM(i) >= 0), i, ARG1, s_alarm);
  SYSCALL(j = alarm(INUM(i)););
  return MAKINUM(j);
}
# ifndef AMIGA
SCM l_pause()
{
  pause();
  return UNSPECIFIED;
}
# endif
#endif /* SIGALRM */

#ifdef _WIN32
# include <windows.h>
#endif
#ifndef AMIGA
# ifndef _Windows
static char s_sleep[] = "sleep";
SCM l_sleep(i)
     SCM i;
{
  unsigned int j = 0;
  ASSERT(INUMP(i) && (INUM(i) >= 0), i, ARG1, s_sleep);
#  ifdef __HIGHC__
  SYSCALL(sleep(INUM(i)););
#  else
#   ifdef _WIN32
  Sleep(INUM(i));
#   else
  SYSCALL(j = sleep(INUM(i)););
#   endif
  return MAKINUM(j);
}
#  endif
# endif
#endif

#ifndef _WIN32
# ifndef GO32
#  ifndef sun
/* int raise P((int sig)); */
static char s_raise[] = "raise";
SCM l_raise(sig)
     SCM sig;
{
  ASSERT(INUMP(sig), sig, ARG1, s_raise);
#   ifdef vms
  return MAKINUM(gsignal((int)INUM(sig)));
#   else
#    ifndef __TURBOC__
#     ifdef STDC_HEADERS
  return kill(getpid (), (int)INUM(sig)) ? BOOL_F : BOOL_T;
#     else
  return raise((int)INUM(sig)) ? BOOL_F : BOOL_T;
#     endif
#    else
  return raise((int)INUM(sig)) ? BOOL_F : BOOL_T;
#    endif
#   endif
}
#  endif
# endif
#endif
#ifdef TICKS
unsigned int tick_count = 0, ticken = 0;
SCM *loc_tick_signal;
void tick_signal()
{
  if (ticken && NIMP(*loc_tick_signal)) {
    ticken = 0;
    apply(*loc_tick_signal, EOL, EOL);
  }
}
static char s_ticks[] = "ticks";
SCM lticks(i)
     SCM i;
{
  SCM j = ticken ? tick_count : 0;
  if (!UNBNDP(i)) ticken = tick_count = INUM(i);
  return MAKINUM(j);
}
#endif

#ifdef SIGHUP
static SIGRETTYPE (*oldhup)();
#endif
static SIGRETTYPE (*oldint)();
#ifdef FLOATS
static SIGRETTYPE (*oldfpe)();
#endif
#ifdef SIGBUS
static SIGRETTYPE (*oldbus)();
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
static SIGRETTYPE (*oldsegv)();
#endif
#ifdef SIGALRM
static SIGRETTYPE (*oldalrm) ();
#endif
#ifdef SIGPIPE
static SIGRETTYPE (*oldpipe) ();
#endif

int dumped = 0;			/* Is this an invocation of unexec exe? */

#ifdef SHORT_ALIGN
typedef short STACKITEM;
#else
typedef long STACKITEM;
#endif
/* See scm.h for definition of P */
void  init_storage P((STACKITEM *stack_start_ptr, long init_heap_size));

void init_scm( iverbose, buf0stdin, init_heap_size )
     int iverbose;
     int buf0stdin;
     long init_heap_size;
{
  STACKITEM i;
  if (2 <= iverbose) init_banner();
  if (!dumped) {
    init_types();
    init_tables();
    init_storage(&i, init_heap_size); /* CONT(rootcont)->stkbse gets set here */
    if (buf0stdin) CAR(def_inp) |= BUF0;
    init_features();
    init_subrs();
    init_io();
    init_scl();
    init_eval();
    init_time();
    init_repl( iverbose );
    init_unif();
  }}

void init_signals()
{
  oldint = signal(SIGINT, int_signal);
#ifdef SIGHUP
  oldhup = signal(SIGHUP, hup_signal);
#endif
#ifdef FLOATS
  oldfpe = signal(SIGFPE, fpe_signal);
#endif
#ifdef SIGBUS
  oldbus = signal(SIGBUS, bus_signal);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  oldsegv = signal(SIGSEGV, segv_signal);
#endif
#ifdef SIGALRM
  alarm(0);			/* kill any pending ALRM interrupts */
  oldalrm = signal(SIGALRM, alrm_signal);
#endif
#ifdef SIGPIPE
  oldpipe = signal(SIGPIPE, SIG_IGN);
#endif
#ifdef ultrix
  siginterrupt(SIGINT, 1);
  siginterrupt(SIGALRM, 1);
  siginterrupt(SIGHUP, 1);
  siginterrupt(SIGPIPE, 1);
#endif /* ultrix */
}

/* This is used in preparation for a possible fork().  Ignore all
   signals before the fork so that child will catch only if it
   establishes a handler */
void ignore_signals()
{
#ifdef ultrix
  siginterrupt(SIGINT, 0);
  siginterrupt(SIGALRM, 0);
  siginterrupt(SIGHUP, 0);
  siginterrupt(SIGPIPE, 0);
#endif /* ultrix */
  signal(SIGINT, SIG_IGN);
#ifdef SIGHUP
  signal(SIGHUP, SIG_DFL);
#endif
#ifdef FLOATS
  signal(SIGFPE, SIG_DFL);
#endif
#ifdef SIGBUS
  signal(SIGBUS, SIG_DFL);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal(SIGSEGV, SIG_DFL);
#endif
  /* Some documentation claims that ALRMs are cleared accross forks.
     If this is not always true then the value returned by alarm(0)
     will have to be saved and unignore_signals() will have to
     reinstate it. */
  /* This code should be neccessary only if the forked process calls
     alarm() without establishing a handler:
#ifdef SIGALRM
     oldalrm = signal(SIGALRM, SIG_DFL);
#endif */
  /* These flushes are per warning in man page on fork(). */
  fflush(stdout);
  fflush(stderr);
}

void unignore_signals()
{
  signal(SIGINT, int_signal);
#ifdef SIGHUP
  signal(SIGHUP, hup_signal);
#endif
#ifdef FLOATS
  signal(SIGFPE, fpe_signal);
#endif
#ifdef SIGBUS
  signal(SIGBUS, bus_signal);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal(SIGSEGV, segv_signal);
#endif
#ifdef SIGALRM
  signal(SIGALRM, alrm_signal);
#endif
#ifdef ultrix
  siginterrupt(SIGINT, 1);
  siginterrupt(SIGALRM, 1);
  siginterrupt(SIGHUP, 1);
  siginterrupt(SIGPIPE, 1);
#endif /* ultrix */
}

void restore_signals()
{
#ifdef ultrix
  siginterrupt(SIGINT, 0);
  siginterrupt(SIGALRM, 0);
  siginterrupt(SIGHUP, 0);
  siginterrupt(SIGPIPE, 0);
#endif /* ultrix */
  signal(SIGINT, oldint);
#ifdef SIGHUP
  signal(SIGHUP, oldhup);
#endif
#ifdef FLOATS
  signal(SIGFPE, oldfpe);
#endif
#ifdef SIGBUS
  signal(SIGBUS, oldbus);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  signal(SIGSEGV, oldsegv);
#endif
#ifdef SIGPIPE
  signal(SIGPIPE, oldpipe);
#endif
#ifdef SIGALRM
  alarm(0);			/* kill any pending ALRM interrupts */
  signal(SIGALRM, oldalrm);
#endif
}

int run_scm(argc, argv, iverbose, buf0stdin, initpath)
     int argc;
     char **argv;
     int iverbose;
     int buf0stdin;
     char *initpath;
{
  SCM i;
  do {
    i = 0L;
    if ((2 <= argc) && argv[1] && (0==strncmp("-a", argv[1], 2))) {
      char *str = (0==argv[1][2] && 3 <= argc && argv[2]) ?argv[2]:&argv[1][2];
      do {
	switch (*str) {
	case DIGITS:
	  i = i * 10 + (*str - '0');
	  if (i <= 10000L) continue; /* the size limit should match Init.scm */
	default:
	  i = 0L;
	}
	break;
      } while (* ++str);
    }
    init_scm(iverbose, buf0stdin, (0 >= i) ? 0L : 1024L * i); /* size in Kb */
    progargs = EOL;
    progargs = makfromstrs(argc, argv);

    if (!dumped) {
#ifdef HAVE_DYNL
      init_dynl();
#endif
#ifdef INITS
      INITS;			/* call initialization of extension files */
#endif
    }
    init_signals();
    i = repl_driver(initpath);
    restore_signals();
#ifdef TICKS
    ticken = 0;
#endif
#ifdef FINALS
    FINALS;			/* call shutdown of extensions files */
#endif /* for compatability with older modules */
    /* call finalization of user extensions */
    while (num_finals--) (finals[num_finals])();
    final_repl();
    free_storage();		/* free all allocated memory */
    if (i) break;
    dumped = 0;
    if (2 <= iverbose) fputs(";RESTART\n", stderr);
  } while (!0);
  if (2 <= iverbose) fputs(";EXIT\n", stderr);
  fflush(stderr);
  return (int)INUM(i);
}

#ifdef vms
# define SYSTNAME "vms"
#endif
#ifdef unix
# define DIRSEP "/"
# ifndef MSDOS			/* DJGPP defines both */
#  define SYSTNAME "unix"
# endif
#endif
#ifdef MWC
# define SYSTNAME "coherent"
# define DIRSEP "/"
#endif
#ifdef _Windows
# define SYSTNAME "windows"
# define DIRSEP "\\"
#else
# ifdef MSDOS
#  define SYSTNAME "ms-dos"
#  ifndef unix
#   define DIRSEP "\\"
#  endif
# endif
#endif
#ifdef __EMX__
# define SYSTNAME "os/2"
# define DIRSEP "\\"
#endif
#ifdef __IBMC__
# define SYSTNAME "os/2"
# define DIRSEP "\\"
#endif
#ifdef THINK_C
# define SYSTNAME "thinkc"
# define DIRSEP ":"
#endif
#ifdef AMIGA
# define SYSTNAME "amiga"
# define DIRSEP "/"
#endif
#ifdef atarist
# define SYSTNAME "atarist"
# define DIRSEP "\\"
#endif
#ifdef mach
# define SYSTNAME "mach"
# define DIRSEP "/"
#endif
#ifdef ARM_ULIB
# define SYSTNAME "acorn"
#endif
#ifdef nosve
# define INIT_FILE_NAME "Init_scm";
# define DIRSEP "."
#endif

SCM softtype()
{
#ifdef nosve
  return CAR(intern("nosve", 5));
#else
  return CAR(intern(SYSTNAME, sizeof SYSTNAME/sizeof(char) -1));
#endif
}

/* Concatentate str2 onto str1 at position n and return concatenated
   string if file exists; 0 otherwise. */

char *scm_cat_path(str1, str2, n)
     char *str1;
     const char *str2;
     long n;
{
  if (!n) n = strlen(str2);
  if (str1)
    {
      long len = strlen(str1);
      str1 = (char *)realloc(str1, (sizet)(len + n + 1));
      if (!str1) return 0;
      strncat(str1 + len, str2, n);
      return str1;
    }
  str1 = (char *)malloc((sizet)(n + 1));
  if (!str1) return 0;
  str1[0] = 0;
  strncat(str1, str2, n);
  return str1;
}

char *scm_try_path(path)
     char *path;
{
  FILE *f;
  /* fprintf(stderr, "Trying %s\n", path);fflush(stderr); */
  if (!path) return 0;
  SYSCALL(f = fopen(path, "r"););
  if (f) {
    fclose(f);
    return path;
  }
  free(path);
  return 0;
}

char *scm_sep_init_try(path, sep, initname)
     char *path;
     const char *sep, *initname;
{
  if (path) path = scm_cat_path(path, sep, 0L);
  if (path) path = scm_cat_path(path, initname, 0L);
  return scm_try_path(path);
}

#ifdef MSDOS
char *dld_find_executable(file)
     const char *file;
{
  return scm_cat_path(0L, file, 0L);
}
#endif

#ifndef INIT_FILE_NAME
# define INIT_FILE_NAME "Init.scm"
#endif
#ifndef DIRSEP
# define DIRSEP "/"
#endif
#ifndef GENERIC_NAME
# define GENERIC_NAME "scm"
#endif

/* Given dld_find_executable()'s best guess for the pathname of this
   executable, find (and verify the existence of) initname in the
   implementation-vicinity of this program.  Returns a newly allocated
   string if successful, 0 if not */

char *scm_find_impl_file(exec_path, generic_name, initname, sep)
     char *exec_path;
     const char *generic_name, *initname, *sep;
{
  char *sepptr = strrchr(exec_path, sep[0]);
  char *extptr = exec_path + strlen(exec_path);
  char *path = 0;
  if (sepptr) {
    long sepind = sepptr - exec_path + 1L;

    /* In case exec_path is in the source directory, look first in
       exec_path's directory. */
    path = scm_cat_path(0L, exec_path, sepind - 1L);
    path = scm_sep_init_try(path, sep, initname);
    if (path) return path;

#ifdef MSDOS
    if (!strcmp(extptr - 4, ".exe") || !strcmp(extptr - 4, ".com") ||
	!strcmp(extptr - 4, ".EXE") || !strcmp(extptr - 4, ".COM"))
      extptr = extptr - 4;
#endif

    if (generic_name &&
	!strncmp(exec_path + sepind, generic_name, extptr - exec_path))
      generic_name = 0;

    /* If exec_path is in directory "exe" or "bin": */
    path = scm_cat_path(0L, exec_path, sepind - 1L);
    sepptr = path + sepind - 4;
    if (!strcmp(sepptr, "exe") || !strcmp(sepptr, "bin") ||
	!strcmp(sepptr, "EXE") || !strcmp(sepptr, "BIN")) {
      char *peer;

      /* Look for initname in peer directory "lib". */
      if (path) {
	strncpy(sepptr, "lib", 3);
	path = scm_sep_init_try(path, sep, initname);
	if (path) return path;
      }

      /* Look for initname in peer directories "lib" and "src" in
	 subdirectory with the name of the executable (sans any type
	 extension like .EXE). */
      for(peer="lib";!0;peer="src") {
	path = scm_cat_path(0L, exec_path, extptr - exec_path + 0L);
	if (path) {
	  strncpy(path + sepind - 4, peer, 3);
	  path[extptr - exec_path] = 0;
	  path = scm_sep_init_try(path, sep, initname);
	  if (path) return path;
	}
	if (!strcmp(peer,"src")) break;
      }

      if (generic_name) {

	/* Look for initname in peer directories "lib" and "src" in
	   subdirectory with the generic name. */
	for(peer="lib";!0;peer="src") {
	  path = scm_cat_path(0L, exec_path, sepind);
	  if (path) {
	    strncpy(path + sepind - 4, "lib", 3);
	    path = scm_cat_path(path, generic_name, 0L);
	    path = scm_sep_init_try(path, sep, initname);
	    if (path) return path;
	  }
	  if (!strcmp(peer,"src")) break;
	}}}

#ifdef MSDOS
    if (strlen(extptr)) {
      /* If exec_path has type extension, look in a subdirectory with
	 the name of the executable sans the executable file's type
	 extension. */
      path = scm_cat_path(0L, exec_path, extptr - exec_path + 0L);
      path = scm_sep_init_try(path, sep, initname);
      if (path) return path;

      if (generic_name) {

	/* Also look in generic_name subdirectory. */
	path = scm_cat_path(0L, exec_path, sepind);
	if (path) path = scm_cat_path(path, generic_name, 0L);
	path = scm_sep_init_try(path, sep, initname);
	if (path) return path;
      }}
#endif
  }
  else {

    /* We don't have a parse-able exec_path.  The only path to try is
       just initname. */
    path = scm_cat_path(0L, initname, 0L);
    if (path) path = scm_try_path(path);
    if (path) return path;
  }
  return 0;
}

#ifndef RTL
char *execpath = 0;
int main( argc, argv )
     int argc;
     char **argv;
{
  int retval, buf0stdin = 0;
  char *getenvpath, *implpath = 0;

# ifndef nosve
  getenvpath = getenv("SCM_INIT_PATH");
  if (getenvpath) implpath = scm_cat_path(0L, getenvpath, 0L);
  if (implpath) {

    /* The value of the environment variable supersedes other
       locations, as long as the file exists. */
    implpath = scm_try_path(implpath);
    if (!implpath) {
      fputs("Value of SCM_INIT_PATH (=\"", stderr);
      fputs(getenvpath, stderr);
      fputs("\") not found; Trying elsewhere\n", stderr);
    }
  }
# endif

  if (!implpath) {
    execpath = dld_find_executable(argv[0]);
    if (execpath) {
      /* fprintf(stderr, "dld found exe \"%s\"\n", execpath); fflush(stderr); */
      implpath = scm_find_impl_file(execpath,
				    GENERIC_NAME, INIT_FILE_NAME, DIRSEP);
      /* fprintf(stderr, "scm_find_impl_file returned \"%s\"\n", implpath); fflush(stderr); */
    }
# ifdef IMPLINIT
    if (!implpath) implpath = scm_cat_path(0L, IMPLINIT, 0L);
# endif
  }
# ifndef GO32
  if (isatty(fileno(stdin))) {
    buf0stdin = !0;		/* stdin gets marked BUF0 in init_scm() */
#  ifndef NOSETBUF
#   ifndef _DCC
#    ifndef ultrix
#     ifndef __WATCOMC__
#      ifndef THINK_C
#       if (__TURBOC__ != 1)
#        ifndef _Windows
    setbuf(stdin, 0);		/* Often setbuf isn't actually required */
#        endif
#       endif
#      endif
#     endif
#    endif
#   endif
  }
#  endif
# endif
  retval = run_scm(argc, argv,
		   (isatty(fileno(stdin)) && isatty(fileno(stdout)))
		   ? (argc <= 1) ? 2 : 1 : 0,
		   buf0stdin,
		   implpath ? implpath : "");
  if (implpath) free(implpath);
  if (execpath) free(execpath);
  return retval;
}
#endif

#ifndef _Windows
char s_system[] = "system";
SCM lsystem(cmd)
     SCM cmd;
{
  ASSERT(NIMP(cmd) && STRINGP(cmd), cmd, ARG1, s_system);
  ignore_signals();
# ifdef AZTEC_C
  cmd = MAKINUM(Execute(CHARS(cmd), 0, 0));
# else
  cmd = MAKINUM(0L+system(CHARS(cmd)));
# endif
  unignore_signals();
  return cmd;
}
#endif

char s_getenv[] = "getenv";
char *getenv();
SCM lgetenv(nam)
     SCM nam;
{
  char *val;
  ASSERT(NIMP(nam) && STRINGP(nam), nam, ARG1, s_getenv);
  val = getenv(CHARS(nam));
  if (!val) return BOOL_F;
  return makfrom0str(val);
}

#ifdef vms
# include <descrip.h>
# include <ssdef.h>
char s_ed[] = "ed";
SCM ed(fname)
     SCM fname;
{
  struct dsc$descriptor_s d;
  ASSERT(NIMP(fname) && STRINGP(fname), fname, ARG1, s_ed);
  d.dsc$b_dtype = DSC$K_DTYPE_T;
  d.dsc$b_class = DSC$K_CLASS_S;
  d.dsc$w_length = LENGTH(fname);
  d.dsc$a_pointer = CHARS(fname);
  /* I don't know what VMS does with signal handlers across the
     edt$edit call. */
  ignore_signals();
  edt$edit(&d);
  unignore_signals();
  return fname;
}
SCM vms_debug()
{
  lib$signal(SS$_DEBUG);
  return UNSPECIFIED;
}
#endif

static iproc subr0s[] = {
	{"software-type", softtype},
	{"scm_init_extensions", scm_init_extensions},
#ifdef vms
	{"vms-debug", vms_debug},
#endif
#ifdef SIGALRM
# ifndef AMIGA
	{"pause", l_pause},
# endif
#endif
	{0, 0}};
static iproc subr1s[] = {
	{s_getenv, lgetenv},
#ifndef _Windows
	{s_system, lsystem},
#endif
#ifdef vms
	{s_ed, ed},
#endif
#ifdef SIGALRM
	{s_alarm, lalarm},
#endif
#ifndef AMIGA
# ifndef _Windows
	{s_sleep, l_sleep},
# endif
#endif
#ifndef GO32
# ifndef sun
#  ifndef _WIN32
	{s_raise, l_raise},
#  endif
# endif
#endif
	{0, 0}};

SCM *loc_features;
void add_feature(str)
     char* str;
{
  *loc_features = cons(CAR(intern(str, strlen(str))), *loc_features);
}
void init_features()
{
  loc_features = &CDR(sysintern("*features*", EOL));
  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
#ifdef TICKS
  loc_tick_signal = &CDR(sysintern("ticks-interrupt", UNDEFINED));
  make_subr(s_ticks, tc7_subr_1o, lticks);
#endif
#ifdef RECKLESS
  add_feature("reckless");
#endif
#ifndef _Windows
  add_feature(s_system);
#endif
#ifdef vms
  add_feature(s_ed);
#endif
  sysintern("*scm-version*", makfrom0str(SCMVERSION));
}
