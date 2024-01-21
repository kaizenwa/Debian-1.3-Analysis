/* Copyright (C) 1994, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* ifilter.h */
/* Filter creation utilities for Ghostscript */
/* Requires oper.h, stream.h, strimpl.h */
#include "istream.h"
#include "ivmspace.h"

/* A filter will be allocated in global VM iff the source/target */
/* and all relevant parameters (if any) are in global VM. */
/* space is the max of the space attributes of all relevant parameters. */
int filter_read(P5(os_ptr op, int npop, const stream_template *template,
		   stream_state *st, uint space));
int filter_write(P5(os_ptr op, int npop, const stream_template *template,
		    stream_state *st, uint space));

/*
 * Define the state of a procedure-based stream.
 * Note that procedure-based streams are defined at the Ghostscript
 * interpreter level, unlike all other stream types which depend only
 * on the stream package and the memory manager.
 */
typedef struct stream_proc_state_s {
	stream_state_common;
	bool eof;
	uint index;		/* current index within data */
	ref proc;
	ref data;
} stream_proc_state;
#define private_st_stream_proc_state() /* in zfproc.c */\
  gs_private_st_complex_only(st_sproc_state, stream_proc_state,\
    "procedure stream state", sproc_clear_marks, sproc_enum_ptrs, sproc_reloc_ptrs, 0)
