/*************************************************
*     Exim - an Internet mail transport agent    *
*************************************************/

/* Copyright (c) University of Cambridge 1995 - 1997 */
/* See the file NOTICE for conditions of use and distribution. */


/* Source files for exim all #include this header, which drags in everything
that is needed. They don't all need everything, of course, but it's far too
messy to have each one importing its own list, and anyway, most of them need
most of these includes. */

/* First of all include the os-specific header, which might set things that
are needed by any of the other headers, including system headers. */

#include "os.h"


/* ANSI C standard includes */

#include <ctype.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Unix includes */

#include <errno.h>
#if	defined(__svr4__) && defined(__sparc) && ! defined(__EXTENSIONS__)
#define __EXTENSIONS__  /* so that SunOS 5 gets NGROUPS_MAX */
#include <limits.h>
#undef  __EXTENSIONS__
#else
#include <limits.h>
#endif
#include <dirent.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>
#include <syslog.h>
#ifndef NO_SYSEXITS      /* some OS don't have this */
#include <sysexits.h>
#endif
#include <sys/time.h>
#include <sys/param.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#ifndef  SIOCGIFCONF	 /* HACK for SunOS 5 */
#include <sys/sockio.h>
#endif
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <unistd.h>
#include <utime.h>
#include <net/if.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <resolv.h>

/* These three are to support the IP option logging code. Linux is
different to everyone else. */

#include <netinet/in_systm.h>
#include <netinet/ip.h>

#ifndef LINUX_IP_OPTIONS
#include <netinet/ip_var.h>
#endif


/* The ident.h header defines __P(x) if it is not already defined (for handling
prototypes). Unfortunately, on some systems, db.h (which is included from
Exim's dbhdr.h for native Berkeley DB use) also defines it, without checking
for a previous definition. This leads to a warning message. However, on other
systems, db.h doesn't define it, expecting it to be done by other headers. To
avoid warnings and nevertheless keep things working in all cases, we arrange
to undefine __P if and only if ident.h defines it. */

#if defined(__P)
#define __P_WAS_DEFINED_BEFORE_IDENT
#endif

#include "libident/ident.h"

#if defined(__P) && ! defined (__P_WAS_DEFINED_BEFORE_IDENT)
#undef __P
#endif

/* One operating system uses a different type for the 2nd argument of select().
Its os.h file defines SELECT_ARG2_TYPE. For the rest, define a default here. */

#ifndef SELECT_ARG2_TYPE
#define SELECT_ARG2_TYPE fd_set
#endif

/* One operating system defines a different type for the yield of inet_addr().
In Exim code, its value is always assigned to the s_addr members of address
structures. Casting the yield to the type of s_addr should fix the problem,
since the size of the data is correct. Just in case this ever has to be
changed, use a macro for the type, and define it here so that it is possible to
use different values for specific OS if ever necessary. */

#ifndef S_ADDR_TYPE
#define S_ADDR_TYPE u_long
#endif

/* The header from the regexp package */

#include "../regexp/regexp.h"

/* Exim includes are in several files */

#include "macros.h"
#include "config.h"
#include "dbhdr.h"
#include "structs.h"
#include "globals.h"
#include "functions.h"

/* If LOG_FILE_PATH or PID_FILE_PATH have not been defined, set them to
the null string. */

#ifndef LOG_FILE_PATH
  #define LOG_FILE_PATH ""
#endif
#ifndef PID_FILE_PATH
  #define PID_FILE_PATH ""
#endif

/* End of exim.h */
