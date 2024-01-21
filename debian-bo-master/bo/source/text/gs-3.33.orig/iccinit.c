/* Copyright (C) 1993 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* iccinit.c */
/* Support for compiled initialization files */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "store.h"
#include "stream.h"
#include "files.h"

/* Import the initialization string from gs_init.c. */
extern const byte far_data gs_ccinit_string[];
extern const uint far_data gs_ccinit_string_sizeof;

/* This substitutes for the normal zinitialfile routine in gsmain.c. */
private int
zccinitialfile(register os_ptr op)
{	int code;
	push(1);
	code = file_read_string(gs_ccinit_string, gs_ccinit_string_sizeof,
				NULL, (ref *)op);
	if ( code < 0 )
		pop(1);
	return code;
}

/* Generate the operator initialization table. */
BEGIN_OP_DEFS(ccinit_op_defs) {
	{"0.initialfile", zccinitialfile},
END_OP_DEFS(0) }
