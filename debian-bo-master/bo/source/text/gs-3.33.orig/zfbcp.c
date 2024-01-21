/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* zfbcp.c */
/* (T)BCP filter creation */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "stream.h"
#include "strimpl.h"
#include "sfilter.h"
#include "ifilter.h"

/* Define null handlers for the BCP out-of-band signals. */
private int
no_bcp_signal_interrupt(stream_state *st)
{	return 0;
}
private int
no_bcp_request_status(stream_state *st)
{	return 0;
}

/* <source> BCPEncode/filter <file> */
private int
zBCPE(os_ptr op)
{	return filter_write(op, 0, &s_BCPE_template, NULL, 0);
}

/* <target> BCPDecode/filter <file> */
private int
zBCPD(os_ptr op)
{	stream_BCPD_state state;
	state.signal_interrupt = no_bcp_signal_interrupt;
	state.request_status = no_bcp_request_status;
	return filter_read(op, 0, &s_BCPD_template, (stream_state *)&state, 0);
}

/* <source> TBCPEncode/filter <file> */
private int
zTBCPE(os_ptr op)
{	return filter_write(op, 0, &s_TBCPE_template, NULL, 0);
}

/* <target> TBCPDecode/filter <file> */
private int
zTBCPD(os_ptr op)
{	stream_BCPD_state state;
	state.signal_interrupt = no_bcp_signal_interrupt;
	state.request_status = no_bcp_request_status;
	return filter_read(op, 0, &s_TBCPD_template, (stream_state *)&state, 0);
}

/* ------ Initialization procedure ------ */

BEGIN_OP_DEFS(zfbcp_op_defs) {
		op_def_begin_filter(),
	{"1BCPEncode", zBCPE},
	{"1BCPDecode", zBCPD},
	{"1TBCPEncode", zTBCPE},
	{"1TBCPDecode", zTBCPD},
END_OP_DEFS(0) }
