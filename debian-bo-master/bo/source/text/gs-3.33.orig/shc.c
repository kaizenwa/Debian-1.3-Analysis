/* Copyright (C) 1992, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* shc.c */
/* Support code for shc.h */
#include "std.h"
#include "scommon.h"
#include "shc.h"

/* ------ Encoding ------ */

/* Empty the 1-word buffer onto the output stream. */
/* q has already been incremented. */
void
hc_put_code_proc(bool reverse_bits, byte *q, uint cw)
{
#define cb(n) ((byte)(cw >> (n * 8)))
	if ( reverse_bits )
	{
#if hc_bits_size > 16
		q[-3] = sbits_reverse_bits[cb(3)];
		q[-2] = sbits_reverse_bits[cb(2)];
#endif
		q[-1] = sbits_reverse_bits[cb(1)];
		q[0] = sbits_reverse_bits[cb(0)];
	}
	else
	{
#if hc_bits_size > 16
		q[-3] = cb(3);
		q[-2] = cb(2);
#endif
		q[-1] = cb(1);
		q[0] = cb(0);
	}
#undef cb
}

/* Put out any final bytes. */
/* Note that this does a store_state, but not a load_state. */
byte *
hc_put_last_bits_proc(stream_hc_state *ss, byte *q, uint bits, int bits_left)
{	while ( bits_left < hc_bits_size )
	{	byte c = (byte)(bits >> (hc_bits_size - 8));
		if ( ss->FirstBitLowOrder )
		  c = sbits_reverse_bits[c];
		*++q = c;
		bits <<= 8;
		bits_left += 8;
	}
	ss->bits = bits;
	ss->bits_left = bits_left;
	return q;
}
