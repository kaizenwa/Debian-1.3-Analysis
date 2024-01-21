/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* zhsb.c */
/* HSB color operators */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "igstate.h"
#include "store.h"
#include "gshsb.h"

/* - currenthsbcolor <hue> <saturation> <brightness> */
private int
zcurrenthsbcolor(register os_ptr op)
{	float par[3];
	gs_currenthsbcolor(igs, par);
	push(3);
	make_reals(op - 2, par, 3);
	return 0;
}

/* <hue> <saturation> <brightness> sethsbcolor - */
private int
zsethsbcolor(register os_ptr op)
{	float par[3];
	int code;
	if (	(code = num_params(op, 3, par)) < 0 ||
		(code = gs_sethsbcolor(igs, par[0], par[1], par[2])) < 0
	   )
		return code;
	make_null(&istate->colorspace.array);
	pop(3);
	return 0;
}

/* ------ Initialization procedure ------ */

BEGIN_OP_DEFS(zhsb_op_defs) {
	{"0currenthsbcolor", zcurrenthsbcolor},
	{"3sethsbcolor", zsethsbcolor},
END_OP_DEFS(0) }
