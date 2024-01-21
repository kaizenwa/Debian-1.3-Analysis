/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* istream.h */
/* Interpreter-level stream procedures */
/* Requires scommon.h, ostack.h */

/* Procedures exported by zfproc.c */
	/* for zfilter.c - procedure stream initialization */
int sread_proc(P2(ref *, stream **));
int swrite_proc(P2(ref *, stream **));
	/* for interp.c, zfileio.c, zpaint.c - handle a procedure */
	/* callback or an interrupt */
int s_handle_read_exception(P4(int, const ref *, const ref *,
  int (*)(P1(os_ptr))));
int s_handle_write_exception(P4(int, const ref *, const ref *,
  int (*)(P1(os_ptr))));
