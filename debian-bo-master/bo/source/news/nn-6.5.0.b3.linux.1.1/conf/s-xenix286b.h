/*
 *	For SCO XENIX 286
 *	From: Michael K. Brady, michael@jewell.UUCP
 *	Uses termcap!
 */

#define NO_DIRENT_H
#include "s-sys5.h"

#undef	USE_TERMINFO			/* use TERMCAP */
#undef TERMLIB
#define TERMLIB	-ltermlib

#undef	SIGNAL_HANDLERS_ARE_VOID	/* */

#define	HAVE_DIRECTORY			/* */
#include <sys/ndir.h>			/* XENIX */
typedef struct direct Direntry;		/* XENIX */

#undef	HAVE_MKDIR			/* */

/*
 *	Specify the default mailer to be invoked by nnmail
 */

#undef MAILX
#define	MAILX		"/usr/bin/smail"	/* You may disagree! */

/*
 *	Define standard compiler flags here:
 */

#define COMPILER_FLAGS -LARGE -Ox -Ml2t32 -F 8000 -SEG 1000

/*
 *	If your system requires other libraries when linking nn
 *	specify them here:
 */

#define EXTRA_LIB -lx
