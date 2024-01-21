/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* idebug.h */
/* Prototypes for debugging procedures in idebug.c */

/* Print individual values. */
#define debug_print_name(pnref)\
  debug_print_string((pnref)->value.pname->string_bytes,\
		     (pnref)->value.pname->string_size)
void debug_print_ref(P1(const ref *));

/* Dump regions of memory. */
void debug_dump_one_ref(P1(const ref *));
void debug_dump_refs(P3(const ref *from, uint size, const char *msg));
/*void debug_dump_stack(P2(const ref_stack *pstack, const char *msg));*/
void debug_dump_array(P1(const ref *array));
