/* Copyright (C) 1994, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* iscannum.h */
/* Interface to Ghostscript number scanner */

/* Scan a number.  If the number consumes the entire string, return 0; */
/* if not, set *psp to the first character beyond the number and return 1. */
int scan_number(P5(const byte *sp, const byte *end, int sign, ref *pref,
  const byte **psp));
