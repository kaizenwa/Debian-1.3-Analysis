/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* sdctc.c */
/* Code common to DCT encoding and decoding streams */
#include "stdio_.h"
#include "jpeglib.h"
#include "strimpl.h"
#include "sdct.h"

public_st_DCT_state();
/* GC procedures */
private ENUM_PTRS_BEGIN(dct_enum_ptrs) return 0;
	ENUM_CONST_STRING_PTR(0, stream_DCT_state, Markers);
ENUM_PTRS_END
private RELOC_PTRS_BEGIN(dct_reloc_ptrs) {
	RELOC_CONST_STRING_PTR(stream_DCT_state, Markers);
} RELOC_PTRS_END
