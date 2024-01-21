/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* windows_.h */
/* Wrapper for windows.h */

#define STRICT
#include <windows.h>

#ifndef __WATCOMC__
	/* Define null equivalents of the Watcom 32-to-16-bit glue. */
#  define AllocAlias16(ptr) ((DWORD)(ptr))
#  define FreeAlias16(dword) /* */
#  define MK_FP16(fp32) ((DWORD)(fp32))
#  define MK_FP32(fp16) (fp16)
#  define GetProc16(proc, ptype) (proc)
#  define ReleaseProc16(cbp) /* */
#endif

/* Substitute for special "far" library procedures under Win32. */
#ifdef __WIN32__
#  undef _fstrtok
#  define _fstrtok(str, set) strtok(str, set)
#endif
