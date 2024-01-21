/*
 *	Definitions for generic SVR4/386
 */

#include "s-sys5.h"

#define HAVE_JOBCONTROL				/* */
#undef	DETACH_TERMINAL				/* */
#define	DETACH_TERMINAL setsid();		/* */
#define HAVE_TRUNCATE				/* */
#define HAVE_SYSLOG				/* */
#define RESIZING				/* */
#define SYSV_RESIZING				/* */
#define NNTP_EXTRA_LIB	-lsocket -lnsl


#define HAVE_STRING_H
#define HAVE_UNISTD_H
#define HAVE_STDLIB_H
#define HAVE_MEMORY_H
#define HAVE_FCNTL_H

#include <limits.h>
#ifdef NGROUPS_MAX
# define NGROUPS NGROUPS_MAX
# define HAVE_MULTIGROUP				/* */
# define GIDSET_TYPE gid_t
#endif
