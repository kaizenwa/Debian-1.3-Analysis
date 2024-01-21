/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* ilevel.h */
/* Interpreter language level interface for Ghostscript */

/* The current interpreter language level (1 or 2) */
extern ref ref_language_level;
#define level2_enabled ((int)ref_language_level.value.intval == 2)
