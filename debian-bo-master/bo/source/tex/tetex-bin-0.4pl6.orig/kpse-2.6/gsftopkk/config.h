/* Include <stdlib.h>, if it exists. */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* Always include this one. */
#include <stdio.h>

/* Define I_STDARG if there's a stdarg.h. */
#ifdef STDC_HEADERS
#define I_STDARG
#endif

/* Include either <string.h> or <strings.h> here. */
#ifdef HAVE_STRING_H
#include <string.h>
#else
#include <strings.h>
#endif

/* Include <memory.h>, if appropriate.  Note that it may have been left
   out even though it exists.  We do this because of conflicts on many
   systems.  But if it works, then it works. */
#ifdef HAVE_MEMORY_H
#include <memory.h>
#endif

/* Include <unistd.h> if it exists. */
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/* If the system has vfork(), then it may require us to include vfork.h */
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

/* Define this if POSIX <dirent.h> instead of old <sys/dir.h>. */
#ifdef HAVE_DIRENT_H
#define POSIX_DIRENT
#endif

#ifndef HAVE_BZERO
#define bzero(s, n)	memset((s), 0, (n))
#endif

#ifdef HAVE_BCMP
#define memcmp	bcmp
#endif

#ifdef HAVE_BCOPY
#ifndef ultrix 
#define memcpy(d, s, n)	bcopy(s, d, n) */
#endif /* !ultrix */
#endif

/* If the system has strchr(), use it; otherwise hope they have index() */
/* Likewise for strrchr() and rindex() */

#ifdef HAVE_INDEX
#define strchr	index
#endif

#ifdef HAVE_RINDEX
#define strrchr	rindex
#endif

/* Use vfork() if it's available; otherwise, fork() */
#ifndef HAVE_VFORK
#define vfork	fork
#endif
