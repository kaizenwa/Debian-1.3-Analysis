/*
 * tclExtdInt.h
 *
 * Standard internal include file for Extended Tcl library..
 *-----------------------------------------------------------------------------
 * Copyright 1991-1995 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: tclExtdInt.h,v 5.3 1995/09/05 07:55:47 markd Exp $
 *-----------------------------------------------------------------------------
 */

#ifndef TCLEXTDINT_H
#define TCLEXTDINT_H

#include <stdio.h>
#include "tcl.h"

/*
 * Version suffix for extended Tcl, this is appended to the standard Tcl
 * version to form the actual extended Tcl version.
 */

#define TCL_EXTD_VERSION_SUFFIX "a-a2"   /* 7.5a-a2 */

typedef void *void_pt;

#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <sys/param.h>
#include <errno.h>

/*
 * Use the real functions, not the Tcl interface that hides signals.
 */
#undef open
#undef read
#undef waitpid
#undef write

/*
 * Boolean constants.
 */
#ifndef TRUE
#    define TRUE   (1)
#    define FALSE  (0)
#endif

/*
 * Used to return argument messages by most commands.
 */
extern char *tclXWrongArgs;

/*
 * Macros to do string compares.  They pre-check the first character before
 * checking of the strings are equal.
 */

#define STREQU(str1, str2) \
        (((str1) [0] == (str2) [0]) && (strcmp (str1, str2) == 0))
#define STRNEQU(str1, str2, cnt) \
        (((str1) [0] == (str2) [0]) && (strncmp (str1, str2, cnt) == 0))

/*
 * Macro to do ctype functions with 8 bit character sets.
 */
#define ISSPACE(c) (isspace ((unsigned char) c))
#define ISDIGIT(c) (isdigit ((unsigned char) c))
#define ISLOWER(c) (islower ((unsigned char) c))

#ifndef UCHAR
#define UCHAR(c) ((unsigned char) (c))
#endif

/*
 * Macro that behaves like strdup, only uses ckalloc.
 */
#define ckstrdup(sourceStr) \
  (strcpy (ckalloc (strlen (sourceStr) + 1), sourceStr))

#endif
