/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* overlay.h */
/* Overlay definitions for MS-DOS / Borland C++ version of Ghostscript */

/* Parameters used for initialization. */
/* K bytes to use for overlay buffer */
/* *** NOTE: Performance will be unacceptable with OVLBUFK < 200. */
#ifndef OVLBUFK				/* might be set in makefile */
#  define OVLBUFK 120
#endif

/* parameters for EMS memory */
/* #define OVEMS */	/* define this to enable use of EMS memory for overlays */
#define OVEMS_HANDLE 0	/* handle argument to _OvrInitEms */
#define OVEMS_FIRST  0	/* first  argument to _OvrInitEms */
#define OVEMS_PAGES  0	/* pages  argument to _OvrInitEms */

/* parameters for extended memory; note that start and length must be longs! */
/* #define OVEXT	/* define this to enable use of extended memory for overlays */
#define OVEXT_START 0L	/* start  argument to _OvrInitExt */
#define OVEXT_LENGTH 0L	/* length argument to _OvrInitExt */
