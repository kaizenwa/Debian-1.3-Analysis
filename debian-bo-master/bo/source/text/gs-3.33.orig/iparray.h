/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* iparray.h */
/* Packed array constructor for Ghostscript */

/*
 * The only reason to put this in a separate header is that it requires
 * both ipacked.h and istack.h; putting it in either one would make it
 * depend on the other one.  There must be a better way....
 */

/* Procedures implemented in zpacked.c */

/* Make a packed array from the top N elements of a stack. */
int make_packed_array(P4(ref *, ref_stack *, uint, client_name_t));
