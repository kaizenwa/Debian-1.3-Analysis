/* Copyright (C) 1989, 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* gserrors.h */
/* Error code definitions */

/* A procedure that may return an error always returns */
/* a non-negative value (zero, unless otherwise noted) for success, */
/* or negative for failure. */
/* We use ints rather than an enum to avoid a lot of casting. */

#define gs_error_unknownerror (-1)		/* unknown error */
#define gs_error_interrupt (-6)
#define gs_error_invalidaccess (-7)
#define gs_error_invalidfileaccess (-9)
#define gs_error_invalidfont (-10)
#define gs_error_ioerror (-12)
#define gs_error_limitcheck (-13)
#define gs_error_nocurrentpoint (-14)
#define gs_error_rangecheck (-15)
#define gs_error_typecheck (-20)
#define gs_error_undefined (-21)
#define gs_error_undefinedfilename (-22)
#define gs_error_undefinedresult (-23)
#define gs_error_VMerror (-25)
#define gs_error_unregistered (-29)

#define gs_error_Fatal (-100)
