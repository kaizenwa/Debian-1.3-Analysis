/* Copyright (C) 1994, 1995 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility to
  anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU Ghostscript General Public License for full details.
  
*/

/* scftab.c */
/* Shared tables for CCITTFax filters */
#include "std.h"
#include "scommon.h"		/* for scf.h */
#include "scf.h"

/* ---------------- Scanning tables ---------------- */

/*
 * cf_byte_run_length[n][b] is number of 1-bits in byte b starting at
 * bit n, numbering the bits in the byte as 07654321.
 * If the run includes the low-order bit (i.e., might be continued
 * into a following byte), the run length is increased by 8.
 */

#define t8(n) n,n,n,n,n+1,n+1,n+2,n+11
#define r8(n) n,n,n,n,n,n,n,n
#define r16(n) r8(n),r8(n)
#define r32(n) r16(n),r16(n)
#define r64(n) r32(n),r32(n)
#define r128(n) r64(n),r64(n)
const byte cf_byte_run_length_0[256] =
{	r128(0), r64(1), r32(2), r16(3), r8(4), t8(5)
};
private const byte far_data rl1[256] =
{	r64(0), r32(1), r16(2), r8(3), t8(4),
	r64(0), r32(1), r16(2), r8(3), t8(4)
};
private const byte far_data rl2[256] =
{	r32(0), r16(1), r8(2), t8(3),
	r32(0), r16(1), r8(2), t8(3),
	r32(0), r16(1), r8(2), t8(3),
	r32(0), r16(1), r8(2), t8(3)
};
private const byte far_data rl3[256] =
{	r16(0), r8(1), t8(2), r16(0), r8(1), t8(2),
	r16(0), r8(1), t8(2), r16(0), r8(1), t8(2),
	r16(0), r8(1), t8(2), r16(0), r8(1), t8(2),
	r16(0), r8(1), t8(2), r16(0), r8(1), t8(2)
};
private const byte far_data rl4[256] =
{	r8(0), t8(1), r8(0), t8(1), r8(0), t8(1), r8(0), t8(1),
	r8(0), t8(1), r8(0), t8(1), r8(0), t8(1), r8(0), t8(1),
	r8(0), t8(1), r8(0), t8(1), r8(0), t8(1), r8(0), t8(1),
	r8(0), t8(1), r8(0), t8(1), r8(0), t8(1), r8(0), t8(1),
};
#define rr8(a,b,c,d,e,f,g,h)\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h,\
  a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h, a,b,c,d,e,f,g,h
private const byte far_data rl5[256] =
{	rr8(0,0,0,0,1,1,2,11)
};
private const byte far_data rl6[256] =
{	rr8(0,0,1,10,0,0,1,10)
};
private const byte far_data rl7[256] =
{	rr8(0,9,0,9,0,9,0,9)
};

const byte *cf_byte_run_length[8] =
{	cf_byte_run_length_0, rl7, rl6, rl5, rl4, rl3, rl2, rl1
};

/* Some C compilers insist on having executable code in every file.... */
void
cfc_dummy(void)
{
}
