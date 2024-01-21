#ifndef _SYS_PARAM_H
# define _SYS_PARAM_H

/* This is defined to be the same as MAX_PATH which is used internally.
   The Posix version is PATH_MAX.  */
#define MAXPATHLEN	(260 - 1 /*NUL*/)

#if 0 /* FIXME: Do we want this?  */
/* The Posix version of this is NAME_MAX.  */
#define MAXNAMLEN	(MAXPATHLEN)
#endif

/* FIXME: Move this to limits.h.  */
#define PATH_MAX	(260 - 1 /*NUL*/)

/* FIXME: Delete this.  */
#define PATHSIZE	259

/* Max number of open files.  The Posix version is OPEN_MAX.  */
#define NOFILE		64

#define HZ		1000

#define MAXHOSTNAMELEN	64

/* Some autoconf'd packages check for endianness.  When cross-building we
   can't run programs on the target.  Fortunately, autoconf supports the
   definition of byte order in sys/param.h (that's us!).
   The values here are the same as used in gdb/defs.h (are the more
   appropriate values?).  */
#define BIG_ENDIAN	4321
#define LITTLE_ENDIAN	1234

/* All known win32 systems are little endian.  */
#define BYTE_ORDER	LITTLE_ENDIAN

/* FIXME: Why is this here?  */
#include <limits.h>

#endif
