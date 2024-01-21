/* System includes for mtools */

#ifndef SYSINCLUDES_H
#define SYSINCLUDES_H

#include "config.h"

#ifdef linux_gnu
/* RMS strikes again */
#ifndef linux
#define linux
#endif
#endif

#if defined __GNUC__ && defined __STDC__
/* gcc -traditional doesn't have PACKED, UNUSED and NORETURN */
#define PACKED __attribute__ ((packed))
# if __GNUC__ == 2 && __GNUC_MINOR__ > 6 || __GNUC__ >= 3
/* gcc 2.6.3 doesn't have "unused" */		/* mool */
#  define UNUSED(x) x __attribute__ ((unused));x
# else
#  define UNUSED(x) x
# endif
#define NORETURN __attribute__ ((noreturn))
#else
#define UNUSED(x) x
#define PACKED /* */
#define NORETURN /* */

#endif


#include <sys/types.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <stdio.h>
#include <ctype.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_LIBC_H
#include <libc.h>
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#else
int getopt();
extern char *optarg;
extern int optind, opterr;
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_SYS_FILE_H
#include <sys/file.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#ifndef sunos
#include <sys/ioctl.h>
#endif
#endif
/* if we don't have sys/ioctl.h, we rely on unistd to supply a prototype
 * for it. If it doesn't, we'll only get a (harmless) warning. The idea
 * is to get mtools compile on as many platforms as possible, but to not
 * suppress warnings if the platform is broken, as long as these warnings do
 * not prevent compilation */

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#ifndef NO_TERMIO
#ifdef HAVE_TERMIO_H
#include <termio.h>
#endif


#ifdef HAVE_SYS_TERMIO_H
#include <sys/termio.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#endif
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#include <sys/stat.h>

#include <errno.h>
extern int errno;
#if !defined netbsd && !defined freebsd
/* NetBSD seems to choke on this, due to a slightly non-standard definition */
extern char *sys_errlist[];
#endif
#include <pwd.h>

/* On AIX, we have to prefer strings.h, as string.h lacks a prototype 
 * for strcasecmp. On most other architectures, it's string.h which seems
 * to be more complete */
#if (defined(aix) && defined (HAVE_STRINGS_H))
# undef HAVE_STRING_H
#endif

#ifdef HAVE_STRING_H
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
# endif
#endif

#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#else

#ifdef HAVE_SYS_SIGNAL_H
#include <sys/signal.h>
#endif

#endif

#ifdef HAVE_UTIME_H
#include <utime.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef linux
#include <linux/fd.h>
#endif


/* missing functions */
#ifndef HAVE_SRANDOM
#define srandom srand48
#endif

#ifndef HAVE_RANDOM
#define random (long)lrand48
#endif

#ifndef HAVE_STRCHR
#define strchr index
#endif

#ifndef HAVE_STRRCHR
#define strrchr rindex
#endif


#define SIG_CAST RETSIGTYPE(*)()

#ifndef HAVE_STRDUP
extern char *strdup(const char *str);
#endif /* HAVE_STRDUP */


#ifndef HAVE_MEMCPY
extern char *memcpy(char *s1, const char *s2, size_t n);
#endif

#ifndef HAVE_MEMSET
extern char *memset(char *s, char c, size_t n);
#endif /* HAVE_MEMSET */


#ifndef HAVE_STRPBRK
extern char *strpbrk(const char *string, const char *brkset);
#endif /* HAVE_STRPBRK */


#ifndef HAVE_STRTOUL
unsigned long strtoul(const char *string, char **eptr, int base);
#endif /* HAVE_STRTOUL */

#ifndef HAVE_STRSPN
size_t strspn(const char *s, const char *accept);
#endif /* HAVE_STRSPN */

#ifndef HAVE_STRCSPN
size_t strcspn(const char *s, const char *reject);
#endif /* HAVE_STRCSPN */

#ifndef HAVE_STRERROR
char *strerror(int errno);
#endif

#ifndef HAVE_ATEXIT
int atexit(void (*function)(void)); 

#ifndef HAVE_ON_EXIT
void myexit(int code) NORETURN;
#define exit myexit
#endif

#endif


#ifndef HAVE_MEMMOVE
#define memmove(DST, SRC, N) bcopy(SRC, DST, N)
#endif

#ifndef HAVE_STRCASECMP
int strcasecmp(const char *s1, const char *s2);
#endif

#ifndef HAVE_STRNCASECMP
#ifdef __BEOS__
int strncasecmp(const char *s1, const char *s2, unsigned int n);
#else
int strncasecmp(const char *s1, const char *s2, size_t n);
#endif
#endif

#ifndef HAVE_GETPASS
char *getpass(const char *prompt);
#endif


#ifndef __STDC__
#ifndef signed
#define signed /**/
#endif 
#endif /* !__STDC__ */

/* prototypes which might be missing on some platforms, even if the functions
 * are present.  Do not declare argument types, in order to avoid conflict
 * on platforms where the prototypes _are_ correct.  Indeed, for most of
 * these, there are _several_ "correct" parameter definitions, and not all
 * platforms use the same.  For instance, some use the const attribute for
 * strings not modified by the function, and others do not.  By using just
 * the return type, which rarely changes, we avoid these problems.
 */
int fflush();
char *strdup();
int strcasecmp();
int strncasecmp();
char *getenv();
unsigned long strtoul();
int pclose();
void exit();
char *getpass();
int atoi();
FILE *fdopen();
FILE *popen();

#ifndef MAXPATHLEN
#ifdef PATH_MAX
#define MAXPATHLEN PATH_MAX
#else
#define MAXPATHLEN 1024
#endif

#endif


#ifndef linux
#undef USE_XDF
#endif

#ifdef NO_XDF
#undef USE_XDF
#endif


#endif
