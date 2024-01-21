/* Copyright (C) 1992, 1993, 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* stdio_.h */
/* Generic substitute for stdio.h */

/* This is here primarily because we must include std.h before */
/* any file that includes sys/types.h. */
#include "std.h"
#include <stdio.h>

#ifdef VMS
/* VMS doesn't have the unlink system call.  Use delete instead. */
#  ifdef __DECC
#    include <unixio.h>
#  endif
#  define unlink(fname) delete(fname)
#else
/* Other systems may or may not declare unlink in stdio.h; */
/* if they do, the declaration will be compatible with this one. */
int unlink(P1(const char *));
#endif

/* Patch a couple of things possibly missing from stdio.h. */
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#  define SEEK_CUR 1
#endif
#ifndef SEEK_END
#  define SEEK_END 2
#endif
