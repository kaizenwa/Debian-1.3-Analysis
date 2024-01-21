/*
 *	This version is for Tower-32 machines.
 */

#define NO_DIRENT_H
#include "s-sys5.h"

#undef	SIGNAL_HANDLERS_ARE_VOID
#undef	HAVE_MKDIR


/* From: iann@storesys.coles.oz.au (Ian Nicholls,L1M11,6088) */

/* Do you have /usr/include/string.h */
#define HAVE_STRING_H /**/

/* Do you have /usr/include/unistd.h */
#define HAVE_UNISTD_H /**/

/* Do you have /usr/include/stdlib.h */
/* #define HAVE_STDLIB_H /**/

/* Do you have /usr/include/memory.h */
#define HAVE_MEMORY_H /**/

/* Do you have /usr/include/fcntl.h */
#define HAVE_FCNTL_H /**/

