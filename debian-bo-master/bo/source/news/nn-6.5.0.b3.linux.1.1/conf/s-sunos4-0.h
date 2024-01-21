/*
 *	This version is for SUNOS 4.0
 */

#define	USE_STRINGS_H

#include "s-bsd4-3.h"

#define HAVE_STRCHR			/* */

#ifdef sun4
#define COMPILER_FLAGS -misalign
#endif

/*
 *	Define if a signal handler has type void (see signal.h)
 */

#define	SIGNAL_HANDLERS_ARE_VOID	/* */

/*
 *	Specify where the Bourne Shell is.
 */

#undef	SHELL
#define SHELL		"/usr/bin/sh"

/*
 *	Window resizing is supported.
 */
#define RESIZING
