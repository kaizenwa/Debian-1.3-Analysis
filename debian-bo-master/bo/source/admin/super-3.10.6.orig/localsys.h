/* The code should compile with either ANSI C or K&R compilers. */

/*
 *      Copyright (c) 1995, 1996 by William Deich.
 *      Written by William Deich.  Not derived from licensed software.

 *    You may distribute under the terms of either the GNU General Public
 *    License or the Artistic License, as specified in the README file.
 */

/* Use __P() to have prototypes in STD C code, and not use
 * prototypes in K&R C: declare functions as:
 *	func_type  funcname __P((arglist));
 */

#include "config.h"

#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

/* We only include <sgtty.h> so we can do close-on-exec on ancient
 * systems (e.g. V7) that implement it via an ioctl instead of fcntl.
 * Don't include it if not needed for this purpose.
 */
#ifndef HAVE_FCNTL_H
#ifdef HAVE_IOCTL_FIOCLEX
#include <sgtty.h>
#endif
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifndef DONT_DECL_ERRNO
extern int errno;
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#define strchr index
#define strrchr rindex
#endif
#ifndef HAVE_MEMCPY
#define memcpy(d, s, n) bcopy((s), (d), (n))
#define memmove(d, s, n) bcopy((s), (d), (n))
#endif
#ifndef HAVE_STRDUP
char *strdup __P((char *));
#endif

#ifdef HAVE_STANDARDS_H
#include <standards.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_BSDTYPES_H
#include <sys/bsdtypes.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#else
pid_t wait(int *);
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#ifdef HAVE_NET_ROUTE_H
#include <net/route.h>
#endif

#ifdef HAVE_NET_IF_H
struct mbuf;		/* To shut up warnings on Digital Unix, which didn't
			 * declare all arguments as cleanly as it might.
			 */
#include <net/if.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#ifndef NGROUPS_MAX

#ifdef NGROUPS
#define NGROUPS_MAX NGROUPS
#else
#define NGROUPS_MAX 1
#endif

#endif

#ifndef _POSIX_VERSION
uid_t getuid();
gid_t getgid();
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_SHADOW_H
#include <shadow.h>
#endif

#ifdef HAVE_SYS_SECURITY_H
#include <sys/security.h>
#endif

#ifdef HAVE_PROT_H
#include <prot.h>
#endif

#ifdef HAVE_AUTH_H
#include <auth.h>
#endif

#ifdef HAVE_SYS_LABEL_H
#include <sys/label.h>
#endif

#ifdef HAVE_SYS_AUDIT_H
#include <sys/audit.h>
#endif

#ifdef HAVE_PWDADJ_H
#include <pwdadj.h>
#endif

#ifdef __clix__
#include <sys/signal.h>
#else
#include <signal.h>
#endif
#ifdef SCO
#include <sys/signal.h>
#endif

#ifndef _SVR4_SOURCE
#ifndef Digital_UNIX
extern int gethostname __P((char *, size_t size));
#endif
#endif

#ifdef HAVE_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>
extern int sysinfo();
#define gethostname(buf, lbuf) (sysinfo(SI_HOSTNAME, (buf), (lbuf)))
#endif

#ifdef _SVR4_SOURCE
#undef NSIG
#define NSIG _sys_nsig
#else
#ifdef _NSIG
#ifndef NSIG
#define NSIG _NSIG
#endif
#endif
#endif

#ifdef HAVE_VOID_SIGNAL
#define SIGNAL_T void
#else
#define SIGNAL_T int
#endif

#include <sys/stat.h>
#ifndef S_IWGRP
#define S_IWGRP 0000020
#endif
#ifndef S_IWOTH
#define S_IWOTH 0000002
#endif

#ifdef HAVE_MEMORY_H
#include <unistd.h>
#endif

#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#ifdef HAVE_SYSLOG_H
#include <syslog.h>
#endif

/* I have troubles with a good value for MAXPATHLEN (if it isn't defined).
 * _POSIX_PATH_MAX is no good because it's only a minimum maximum.
 * pathconf(path, _PC_PATH_MAX) is no good because it's path-dependent.
 * Well, I'll just take the easy way out...
 */
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

/* type returned by setgrent() */
#ifdef Digital_UNIX
typedef int SETGRENT_T;
#else
typedef void SETGRENT_T;
#endif

#ifdef HAVE_SYSCONF
#define MAXFD (sysconf(_SC_OPEN_MAX)-1)
#else
#ifdef HAVE_GETDTABLESIZE
#define MAXFD (getdtablesize()-1)
#else
#define MAXFD 63
#endif
#endif
