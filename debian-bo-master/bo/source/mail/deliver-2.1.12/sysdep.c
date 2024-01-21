/* $Id: sysdep.c,v 1.9 1993/10/28 16:49:51 chip Exp $
 *
 * Routines which are (or might well be) system-dependant.
 * I've put the message routines here since you may need to use
 * the ANSI <stdarg.h> instead of <varargs.h>.
 *
 * $Log: sysdep.c,v $
 * Revision 1.9  1993/10/28  16:49:51  chip
 * Declare function return types, including void.
 *
 * Revision 1.8  1991/11/12  20:44:14  chip
 * Ignore return values of fflush() and sprintf().
 *
 * Revision 1.7  1991/10/23  19:48:08  chip
 * Add errname().
 * Regularize verror() and vsyserr().
 *
 * Revision 1.6  1991/10/23  19:29:46  chip
 * Add clexec().
 *
 * Revision 1.5  1991/09/27  16:21:37  chip
 * Improve appearance of variadic function definitions.
 *
 * Revision 1.4  1991/08/26  17:45:43  chip
 * Don't declare errno.
 * Use strerror().
 *
 * Revision 1.3  1991/06/04  18:25:15  chip
 * Repair mutant formatting done by indent.
 *
 * Revision 1.2  1991/06/04  18:16:28  chip
 * Feature-based configuration.
 *
 * Revision 1.1  91/05/13  18:36:55  chip
 * Initial revision
 * 
 */

#include "deliver.h"
#include <errno.h>

#ifdef HH_STDARG
#include <stdarg.h>
#else
#ifdef HH_VARARGS
#include <varargs.h>
#else
/*
 * Non-portable home-grown varargs.  Use at your own risk.
 * Especially note that if sizeof(int) > sizeof(short), then
 * "va_arg(..,short)" is broken.
 */
typedef char *va_list;
#define va_dcl          int va_alist;
#define va_start(ap)    ap = (char *) &va_alist
#define va_arg(ap,type) *(type *)(ap += sizeof(type), ap - sizeof(type))
#define va_end(ap)		/* nothing */
#endif
#endif

#ifdef HAS_UNAME
#include <sys/utsname.h>
#endif

/*
 * External functions.
 */

#ifdef HAS_USLEEP
extern void usleep();
#else
#ifdef HAS_NAP
extern long nap();
#else
extern unsigned sleep();
#endif
#endif

/*
 * External data.
 */

#ifndef HAS_STRERROR
extern int sys_nerr;
extern char *sys_errlist[];
#endif

/*
 * Locally useful macros.
 */

/* Hide differences between varargs and stdarg. */

#ifdef HH_STDARG
#define FMT_ARGS        char *fmt, ...
#define FMT_DCL
#define FMT_VARS        va_list ap
#define FMT_START       va_start(ap, fmt)
#define FMT_END         va_end(ap)
#else
#define FMT_ARGS        va_alist
#define FMT_DCL         va_dcl
#define FMT_VARS        va_list ap; char *fmt
#define FMT_START       va_start(ap); fmt = va_arg(ap, char *)
#define FMT_END         va_end(ap)
#endif

/*----------------------------------------------------------------------
 * Print a message.
 */

/* VARARGS1 */
void
message(FMT_ARGS)
FMT_DCL
{
    FMT_VARS;

    FMT_START;
    (void) vfprintf(stderr, fmt, ap);
    FMT_END;

    if (errlog)
    {
	errstart();
	FMT_START;
	(void) vfprintf(errlog, fmt, ap);
	FMT_END;
	(void) fflush(errlog);
    }
}

/*----------------------------------------------------------------------
 * Print an error message.
 */

/* VARARGS1 */
void
error(FMT_ARGS)
FMT_DCL
{
    FMT_VARS;

    FMT_START;
    verror(stderr, fmt, ap);
    FMT_END;

    if (errlog)
    {
	errstart();
	FMT_START;
	verror(errlog, fmt, ap);
	FMT_END;
    }
}

void
verror(fp, fmt, ap)
FILE *fp;
char *fmt;
va_list ap;
{
    (void) fprintf(fp, "%s: ", progname);
    (void) vfprintf(fp, fmt, ap);
    (void) fputc('\n', fp);
    (void) fflush(fp);
}

/*----------------------------------------------------------------------
 * Report an error returned from a system call.
 */

/* VARARGS1 */
void
syserr(FMT_ARGS)
FMT_DCL
{
    int e = errno;
    FMT_VARS;

    FMT_START;
    vsyserr(stderr, fmt, ap, e);
    FMT_END;

    if (errlog)
    {
	errstart();
	FMT_START;
	vsyserr(errlog, fmt, ap, e);
	FMT_END;
    }
}

void
vsyserr(fp, fmt, ap, e)
FILE *fp;
char *fmt;
va_list ap;
int e;
{
    (void) fprintf(fp, "%s: ", progname);
    (void) vfprintf(fp, fmt, ap);
    (void) fprintf(fp, ": %s\n", errname(e));
    (void) fflush(fp);
}

/*----------------------------------------------------------------------
 * Return the name of the given system error.
 */

char *
errname(e)
int e;
{
#ifdef HAS_STRERROR

    return strerror(e);

#else

    static char ue[40];

    if (e <= sys_nerr)
	return sys_errlist[e];

    (void) sprintf(ue, "Error %d", e);
    return ue;

#endif
}

/*----------------------------------------------------------------------
 * Set or clear the given file descriptor's close-on-exec flag.
 */

int
clexec(fd, what)
int fd, what;
{
    int flags;

    if ((flags = fcntl(fd, F_GETFD)) == -1)
    {
	syserr("can't get flags for fd %d", fd);
	return -1;
    }

    flags = what ? (flags | FD_CLOEXEC) : (flags & ~FD_CLOEXEC);

    if (fcntl(fd, F_SETFD, flags) == -1)
    {
	syserr("can't set flags for fd %d", fd);
	return -1;
    }

    return 0;
}

/*----------------------------------------------------------------------
 * Sleep for the given number of seconds.
 */

void
snooze(n)
int n;
{
#ifdef HAS_USLEEP
    usleep(n * 10000000L);
#else
#ifdef HAS_NAP
    (void) nap(n * 1000L);
#else
    (void) sleep((unsigned) n);
#endif
#endif
}

/*----------------------------------------------------------------------
 * Get the host name from HOSTFILE.
 */

#ifdef HOSTFILE

char *
gethost()
{
    int fd, rd;
    char *p;
    static char name[32];

    if ((fd = open(HOSTFILE, O_RDONLY)) == -1)
	return NULL;
    rd = read(fd, name, sizeof(name) - 1);
    (void) close(fd);

    if (rd < 1)
	return NULL;
    name[rd] = 0;
    if ((p = strchr(name, '\n')) != NULL)
	*p = 0;

    return (name[0] ? name : NULL);
}

#endif	/* HOSTFILE */

/*----------------------------------------------------------------------
 * Get the host name via the uname() system call.
 */

#ifdef HAS_UNAME

char *
gethost()
{
    static struct utsname u;

    (void) uname(&u);
    return (u.nodename[0] ? u.nodename : NULL);
}

#endif	/* HAS_UNAME */

/*----------------------------------------------------------------------
 * Get the host name via the gethostname() system call.
 */

#ifdef HAS_GETHOSTNAME

char *
gethost()
{
    static char hostname[64];

    if (gethostname(hostname, sizeof(hostname)) == -1)
	return NULL;

    return hostname;
}

#endif	/* HAS_GETHOSTNAME */

/*----------------------------------------------------------------------
 * Return a pre-defined HOSTNAME.
 */

#ifdef HOSTNAME

char *
gethost()
{
    return HOSTNAME;
}

#endif	/* HOSTNAME */

/*----------------------------------------------------------------------
 * Variable-argument-list output, System V style.
 */

#ifndef HAS_VPRINTF

vprintf(fmt, ap)
char *fmt;
va_list ap;
{
    int a, b, c, d, e, f, g, h;

    a = va_arg(ap, int);
    b = va_arg(ap, int);
    c = va_arg(ap, int);
    d = va_arg(ap, int);
    e = va_arg(ap, int);
    f = va_arg(ap, int);
    g = va_arg(ap, int);
    h = va_arg(ap, int);

    (void) printf(fmt, a, b, c, d, e, f, g, h);
}

vfprintf(fp, fmt, ap)
FILE *fp;
char *fmt;
va_list ap;
{
    int a, b, c, d, e, f, g, h;

    a = va_arg(ap, int);
    b = va_arg(ap, int);
    c = va_arg(ap, int);
    d = va_arg(ap, int);
    e = va_arg(ap, int);
    f = va_arg(ap, int);
    g = va_arg(ap, int);
    h = va_arg(ap, int);

    (void) fprintf(fp, fmt, a, b, c, d, e, f, g, h);
}

vsprintf(s, fmt, ap)
char *s;
char *fmt;
va_list ap;
{
    int a, b, c, d, e, f, g, h;

    a = va_arg(ap, int);
    b = va_arg(ap, int);
    c = va_arg(ap, int);
    d = va_arg(ap, int);
    e = va_arg(ap, int);
    f = va_arg(ap, int);
    g = va_arg(ap, int);
    h = va_arg(ap, int);

    (void) sprintf(s, fmt, a, b, c, d, e, f, g, h);
}

#endif	/* !HAS_VPRINTF */

/*----------------------------------------------------------------------
 * Add a new environment variable.
 */

#ifndef HAS_PUTENV

int
putenv(s)
char *s;
{
    static char **env_array;
    static int env_size;
    char *e;
    int i, j;

    if (env_array == NULL)
    {
	for (i = 0; environ[i]; ++i)
	    continue;
	env_size = i + 10;	/* arbitrary */
	env_array = talloc(char *, env_size);
	Copy((char *) env_array, (char *) environ,
	     (int) ((i + 1) * sizeof(char *)));
	environ = env_array;
    }
    else if (environ != env_array)
	message("putenv: warning: someone moved environ!\n");

    if ((e = strchr(s, '=')) != NULL)
	++e;
    else
	e = s + strlen(s);

    j = 0;
    for (i = 0; env_array[i]; ++i)
    {
	if (strncmp(env_array[i], s, e - s) != 0)
	    env_array[j++] = env_array[i];
    }

    if ((j + 1) >= env_size)
    {
	env_size += 10;		/* arbitrary */
	env_array = (char **) srealloc((char *) env_array,
				       env_size * sizeof(char **));
    }

    env_array[j++] = s;
    env_array[j] = NULL;

    environ = env_array;
    return 0;
}

#endif	/* !HAS_PUTENV */

/*----------------------------------------------------------------------
 * Memory copy.
 */

#ifdef NEED_COPYZERO

Copy(dest, src, len)
char *dest;
char *src;
int len;
{
    while (len-- > 0)
	*dest++ = *src++;
}

#endif /* NEED_COPYZERO */

/*----------------------------------------------------------------------
 * Memory clear.
 */

#ifdef NEED_COPYZERO

Zero(dest, len)
char *dest;
int len;
{
    while (len-- > 0)
	*dest++ = 0;
}

#endif /* NEED_COPYZERO */
