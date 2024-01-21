/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* "wrapper" for IJG jerror.c */
#include "stdpre.h"
#include "jversion.h"
#define JMAKE_MSG_STRINGS
#include "jerror.h"
#undef JMAKE_MSG_STRINGS
#undef JCOPYRIGHT
#undef JVERSION
#include "jerrorig.c"
