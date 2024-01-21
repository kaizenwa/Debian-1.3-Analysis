#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	label.c (Make Label)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Stencil a label icon onto a blank (or prepared) button bitmap
 * Subroutines:	btn_StencilLabel()		returns: void
 * Subroutines:	static btn_Stencil()		returns: void
 * Subroutines:	static btn_StencilAlligned()	returns: void
 * Xlib calls:	none
 * Copyright:	1989, 1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		18 March 1989
 *		{1} Doug Mink   cast before right shifting	   4 May 1995
 *		{n} <who> -- <does what> -- <when>
 */

/* one byte bitmask for bits included at index and to the right */
static unsigned char rmask[8] = { 0xff,0xfe,0xfc,0xf8,0xf0,0xe0,0xc0,0x80 };

/* one byte bitmask for bits included at index and to the left */
static unsigned char lmask[8] = { 0x01,0x03,0x07,0x0f,0x1f,0x3f,0x7f,0xff };

/*
 * Subroutine:	btn_StencilLabel
 * Purpose:	Stencil src through msk onto dst
 * Returns:	void
 * Called by:	btn_AddLabel()
 * Uses:	btn_Stencil() or btn_StencilAlligned() below
 * Xlib calls:	none
 * Pre-state:	dst bitmap already contains border pattern
 * Post-state:	dst bitmap has label stenciled on
 * Method:	Determine alignment parameters and call an appropriate
 *		routine to do the stenciling.
 * Note:	src and msk are assumed to be matching bitmaps.
 * Note:	For each byte, bit 0 (0x01) appears on the left.
 * Note:	For each byte, bit 7 (0x80) appears on the right.
 */
void btn_StencilLabel ( src, msk, dst, src_byte_width, dst_byte_width,
		        src_x, src_y, dst_x, dst_y, width, height, inverse )
     unsigned char *src;	/* i: ptr to first byte of pattern */
     unsigned char *msk;	/* i: ptr to same byte as src, but in msk */
     unsigned char *dst;	/* i: ptr to first byte of destination */
     int src_byte_width;	/* i: width in bytes of the src bitmap */
     int dst_byte_width;	/* i: width in bytes of the dst bitmap */
     int src_x, src_y;		/* i: coords of start of stencil in src */
     int dst_x, dst_y;		/* i: coords of start of stencil in dst */
     int width;			/* i: width (cols) to stencil */
     int height;		/* i: number of lines (rows) to stencil */
     int inverse;		/* i: invert the bits for reverse video look */
{
  int byte_offset;		/* l: byte offset of 1st src byte */
  int first_src_bit;		/* l: index of 1st src bit in 1st src byte */
  int first_dst_bit;		/* l: index of 1st dst bit in 1st dst byte */
  int last_src_bit;		/* l: index of last src bit in last src byte */
  int last_dst_bit;		/* l: index of last dst bit in last dst byte */
  int rshift;			/* l: shift to align 1st src byte with dst */
  int lshift;			/* l: shift to align 2nt src byte with dst */
  unsigned char *last_dst;	/* l: ptr to last byte used in first line */
  int first_mask;		/* l: bit mask, with coded sign (see above) */
  int last_mask;		/* l: bit mask, with coded sign (see above) */

  static void btn_Stencil(), btn_StencilInv();
  static void btn_StencilAlligned(),btn_StencilAllignedInv();

  /* advance src and msk to their first used bytes */
  byte_offset = (src_y * src_byte_width) + (src_x / 8);
  src += byte_offset;
  msk += byte_offset;
  /* advance dst to its first used line */
  dst += dst_y * dst_byte_width;
  /* determine byte ptr to last used byte in first used dst row */
  last_dst = dst + (dst_x + width - 1) / 8;
  /* advance dst to its first used byte */
  dst += dst_x / 8;
  /* determine bit indexes of first and last bits in their bytes */
  first_src_bit = src_x & 0x0007;
  first_dst_bit = dst_x & 0x0007;
  last_dst_bit = (dst_x + width - 1) & 0x0007;
  last_src_bit = (src_x + width - 1) & 0x0007;
  /* get appropriate mask for bits used in first dst byte */
  first_mask = (int)rmask[first_dst_bit];
  /* get appropriate mask and compensations for bits used in last dst byte */
  if( dst == last_dst ) {
    first_mask &= (int)lmask[last_dst_bit];
    last_mask = 0;
  } else if( last_dst_bit == 7 ) {
    last_mask = 0;
    ++last_dst;
  } else {
    last_mask = (int)lmask[last_dst_bit];
  }
  /* test bit alignment between dst and src */
  if( first_src_bit == first_dst_bit ) {
    /* call routine for perfect allignment stenciling */
    if( inverse )
      btn_StencilAllignedInv(src, msk, dst, last_dst, first_mask, last_mask,
			     src_byte_width, dst_byte_width, height);
    else
      btn_StencilAlligned(src, msk, dst, last_dst, first_mask, last_mask,
			  src_byte_width, dst_byte_width, height);
  } else {
    /* determine rshift and lshift from bit offsets */
    lshift = first_dst_bit - first_src_bit;
    rshift = 8 - lshift;
    /* encode handling of first and last bytes */
    if( first_dst_bit > first_src_bit )
      first_mask = -first_mask;
    if( last_dst_bit < last_src_bit )
      last_mask = -last_mask;
    /* call routine for imperfect allignment stenciling */
    if( inverse )
      btn_StencilInv(src, msk, dst, last_dst, first_mask, last_mask,
		     rshift, lshift, src_byte_width, dst_byte_width, height);
    else
      btn_Stencil(src, msk, dst, last_dst, first_mask, last_mask,
		  rshift, lshift, src_byte_width, dst_byte_width, height);
  }
}

/*
 * Subroutine:	btn_Stencil(Inv)
 * Purpose:	Stencil src through msk onto dst with imperfect byte alignment.
 * Parameters:	See argument declarations and note the following.
 * Parameter:	first_mask: a one byte bitmask passed as an int
 *		note: first_mask is negated if first_dst_bit > first_src_bit.
 * Parameter:	last_mask: a one byte bitmask passed as an int
 *		note: last_mask is negated if last_dst_bit < last_src_bit.
 *		note: last_mask = 0 if there is no need to do a last byte.
 * Returns:	void
 * Called by:	btn_StencilLabel() above
 * Xlib calls:	none
 * Pre-state:	dst bitmap already contains border pattern
 * Post-state:	dst bitmap has label stenciled on
 * Exception:	Src and dst bytes must not allign (bit offsets must differ).
 * Method:	Basic stencil operation is (dst & ^mask) | (src & mask).
 *		Operation to make dst aligned byte from 2 src bytes is:
 *		 (src1 >> rshift) | (src2 << lshift).
 *		Special masks are applied at beginning and end in case less
 *		than a full byte is to be stenciled.  If no special masking
 *		is needed for a last byte, last_dst covers it and last_mask=0;
 * Note:	src and msk are assumed to be matching bitmaps.
 * Note:	For each byte, bit 0 (0x01) appears on the left.
 * Note:	For each byte, bit 7 (0x80) appears on the right.
 * Note:	~*s >> rshift does not work on SPARK and MIPS, use (0xff - *s)
 */
static void btn_Stencil ( src, msk, dst, last_dst, first_mask, last_mask,
			  rshift, lshift,
			  src_byte_width, dst_byte_width, height )
     unsigned char *src;	/* i: ptr to first byte used of pattern */
     unsigned char *msk;	/* i: ptr to same byte as src, but in msk */
     unsigned char *dst;	/* i: ptr to first byte used of destination */
     unsigned char *last_dst;	/* i: ptr to last byte used in first line */
     int first_mask;		/* i: bit mask, with coded sign (see above) */
     int last_mask;		/* i: bit mask, with coded sign (see above) */
     int rshift;		/* i: shift to align 1st src byte with dst */
     int lshift;		/* i: shift to align 2nt src byte with dst */
     int src_byte_width;	/* i: width in bytes of the src bitmap */
     int dst_byte_width;	/* i: width in bytes of the dst bitmap */
     int height;		/* i: number of line (rows) to stencil */
{
  unsigned char mask, val;	/* l: composite mask and source val */
  register unsigned char *s, *m, *d;
  unsigned char ff = 0xff;		/* -1 for ~ operation */

  /* loop and decrement height as a row counter */
  while( --height >= 0 ) {
    s = src;
    m = msk;
    d = dst;
    /* handle the first (maybe partial) dst byte */
    if( first_mask < 0 ) {
      mask = (*m << lshift) & (unsigned char)(-first_mask);
      *d = (*d & (ff - mask)) | ((*s << lshift) & mask);
    } else {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      mask &= (unsigned char)first_mask;
      val = (*s >> rshift);
      s++;
      val |= (*s << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* handle middle (full) dst bytes */
    while( ++d < last_dst ) {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      val = (*s >> rshift);
      s++;
      val |= (*s << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* handle last (maybe partial) dst byte */
    if( last_mask < 0 ) {
      mask = (*m >> rshift) & (unsigned char)(-last_mask);
      *d = (*d & (ff - mask)) | ((*s >> rshift) & mask);
    } else if( last_mask > 0 ) {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      mask &= (unsigned char)last_mask;
      val = (*s >> rshift);
      s++;
      val |= (*s << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* advance to next line */
    src += src_byte_width;
    msk += src_byte_width;
    dst += dst_byte_width;
    last_dst += dst_byte_width;
  }
}
static void btn_StencilInv ( src, msk, dst, last_dst, first_mask, last_mask,
			     rshift, lshift,
			     src_byte_width, dst_byte_width, height )
     unsigned char *src;	/* i: ptr to first byte used of pattern */
     unsigned char *msk;	/* i: ptr to same byte as src, but in msk */
     unsigned char *dst;	/* i: ptr to first byte used of destination */
     unsigned char *last_dst;	/* i: ptr to last byte used in first line */
     int first_mask;		/* i: bit mask, with coded sign (see above) */
     int last_mask;		/* i: bit mask, with coded sign (see above) */
     int rshift;		/* i: shift to align 1st src byte with dst */
     int lshift;		/* i: shift to align 2nt src byte with dst */
     int src_byte_width;	/* i: width in bytes of the src bitmap */
     int dst_byte_width;	/* i: width in bytes of the dst bitmap */
     int height;		/* i: number of line (rows) to stencil */
{
  unsigned char mask, val;	/* l: composite mask and source val */
  register unsigned char *s, *m, *d;
  unsigned char ff = 0xff;		/* -1 for ~ operation */

  /* loop and decrement height as a row counter */
  while( --height >= 0 ) {
    s = src;
    m = msk;
    d = dst;
    /* handle the first (maybe partial) dst byte */
    if( first_mask < 0 ) {
      mask = (*m << lshift) & (unsigned char)(-first_mask);
      *d = (*d & (ff - mask)) | (((ff - *s) << lshift) & mask);
    } else {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      mask &= (unsigned char)first_mask;
      val = ((unsigned char)(ff - *s) >> rshift);
      s++;
      val |= ((ff - *s) << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* handle middle (full) dst bytes */
    while( ++d < last_dst ) {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      val = ((unsigned char)(ff - *s) >> rshift);
      s++;
      val |= ((ff - *s) << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* handle last (maybe partial) dst byte */
    if( last_mask < 0 ) {
      mask = (*m >> rshift) & (unsigned char)(-last_mask);
      *d = (*d & (ff - mask)) | (((unsigned char)(ff - *s) >> rshift) & mask);
    } else if( last_mask > 0 ) {
      mask = (*m >> rshift);
      m++;
      mask |= (*m << lshift);
      mask &= (unsigned char)last_mask;
      val = ((unsigned char)(ff - *s) >> rshift);
      s++;
      val |= ((ff - *s) << lshift);
      *d = (*d & (ff - mask)) | (val & mask);
    }
    /* advance to next line */
    src += src_byte_width;
    msk += src_byte_width;
    dst += dst_byte_width;
    last_dst += dst_byte_width;
  }
}

/*
 * Subroutine:	btn_StencilAlligned(Inv)
 * Purpose:	Stencil src through msk onto dst with perfect byte alignment.
 * Returns:	void
 * Called by:	btn_StencilLabel() above
 * Xlib calls:	none
 * Pre-state:	dst bitmap already contains border pattern
 * Post-state:	dst bitmap has label stenciled on
 * Exception:	Src and dst bytes must allign (bit for bit).
 * Method:	Basic stencil operation is (dst & ~mask) | (src & mask).
 *		Special masks are applied at beginning and end in case less
 *		than a full byte is to be stenciled.  If no special masking
 *		is needed for a last byte, last_dst covers it and last_mask=0;
 * Note:	src and msk are assumed to be matching bitmaps.
 * Note:	For each byte, bit 0 (0x01) appears on the left.
 * Note:	For each byte, bit 7 (0x80) appears on the right.
 * Note:	~*s >> rshift does not work on SPARK and MIPS, use (0xff - *s)
 */
static void btn_StencilAlligned ( src, msk, dst, last_dst, first_mask,
				  last_mask, src_byte_width, dst_byte_width,
				  height, inverse )
     unsigned char *src;	/* i: ptr to first byte used of pattern */
     unsigned char *msk;	/* i: ptr to same byte as src, but in msk */
     unsigned char *dst;	/* i: ptr to first byte used of destination */
     unsigned char *last_dst;	/* i: ptr to last byte used in first line */
     int first_mask;		/* i: bit mask, with coded sign (see above) */
     int last_mask;		/* i: bit mask, with coded sign (see above) */
     int src_byte_width;	/* i: width in bytes of the src bitmap */
     int dst_byte_width;	/* i: width in bytes of the dst bitmap */
     int height;		/* i: number of line (rows) to stencil */
{
  unsigned char mask;		/* l: composite mask for use on end bytes */
  register unsigned char *s, *m, *d;
  unsigned char ff = 0xff;		/* -1 for ~ operation */

  /* loop and decrement height as a row counter */
  while( --height >= 0 ) {
    s = src;
    m = msk;
    d = dst;
    /* do first byte, use mask in case it is not a full byte */
    mask = (unsigned char)first_mask & *m;
    *d = (*d & (ff - mask)) | (*s & mask);
    /* do middle (full) bytes */
    while( ++d < last_dst ) {
      ++m;
      ++s;
      *d = (*d & (ff - *m)) | (*s & *m);
    }
    /* if there is a partial last byte, do it */
    if( last_mask ) {
      ++m;
      mask = (unsigned char)last_mask & *m;
      ++s;
      *d = (*d & (ff - mask)) | (*s & mask);
    }
    /* advance to next line */
    src += src_byte_width;
    msk += src_byte_width;
    dst += dst_byte_width;
    last_dst += dst_byte_width;
  }
}
static void btn_StencilAllignedInv ( src, msk, dst, last_dst, first_mask,
				    last_mask, src_byte_width, dst_byte_width,
				    height, inverse )
     unsigned char *src;	/* i: ptr to first byte used of pattern */
     unsigned char *msk;	/* i: ptr to same byte as src, but in msk */
     unsigned char *dst;	/* i: ptr to first byte used of destination */
     unsigned char *last_dst;	/* i: ptr to last byte used in first line */
     int first_mask;		/* i: bit mask, with coded sign (see above) */
     int last_mask;		/* i: bit mask, with coded sign (see above) */
     int src_byte_width;	/* i: width in bytes of the src bitmap */
     int dst_byte_width;	/* i: width in bytes of the dst bitmap */
     int height;		/* i: number of line (rows) to stencil */
{
  unsigned char mask;		/* l: composite mask for use on end bytes */
  register unsigned char *s, *m, *d;
  unsigned char ff = 0xff;		/* -1 for ~ operation */

  /* loop and decrement height as a row counter */
  while( --height >= 0 ) {
    s = src;
    m = msk;
    d = dst;
    /* do first byte, use mask in case it is not a full byte */
    mask = (unsigned char)first_mask & *m;
    *d = (*d & (ff - mask)) | ((ff - *s) & mask);
    /* do middle (full) bytes */
    while( ++d < last_dst ) {
      ++m;
      ++s;
      *d = (*d & (ff - *m)) | ((ff - *s) & *m);
    }
    /* if there is a partial last byte, do it */
    if( last_mask ) {
      ++m;
      mask = (unsigned char)last_mask & *m;
      ++s;
      *d = (*d & (ff - mask)) | ((ff - *s) & mask);
    }
    /* advance to next line */
    src += src_byte_width;
    msk += src_byte_width;
    dst += dst_byte_width;
    last_dst += dst_byte_width;
  }
}

#ifdef DEBUG
pchar ( buf, wid, ht )
     char *buf;
     int wid, ht;
{
  int i, j;
  (void)printf("\n");
  for( i=0; i<ht; i++ ) {
    for( j=0; j<wid; j++ ) {
      (void)printf(" %4x", (unsigned char)buf[(i*wid)+j]);
    }
    (void)printf("\n");
  }
}
bchar ( buf, wid, ht )
     char *buf;
     int wid, ht;
{
  int i, j, k, val;
  (void)printf("\n");
  for( i=0; i<ht; i++ ) {
    for( j=0; j<wid; j++ ) {
      k = 0;
      val = (unsigned int)buf[(i*wid)+j];
      if( val & 1 )   k += 10000000;
      if( val & 2 )   k += 1000000;
      if( val & 4 )   k += 100000;
      if( val & 8 )   k += 10000;
      if( val & 16 )  k += 1000;
      if( val & 32 )  k += 100;
      if( val & 64 )  k += 10;
      if( val & 128 ) k += 1;
      (void)printf("%08d", k);
    }
    (void)printf("\n");
  }
}
dchar ( buf, wid, ht )
     char *buf;
     int wid, ht;
{
  int i, j, k, val;
  char foo[10];
  (void)printf("\n");
  for( i=0; i<ht; i++ ) {
    for( j=0; j<wid; j++ ) {
      strcpy(foo, "........");
      val = (unsigned int)buf[(i*wid)+j];
      if( val & 1 ) foo[0] = 'O';
      if( val & 2 ) foo[1] = 'O';
      if( val & 4 ) foo[2] = 'O';
      if( val & 8 ) foo[3] = 'O';
      if( val & 16 ) foo[4] = 'O';
      if( val & 32 ) foo[5] = 'O';
      if( val & 64 ) foo[6] = 'O';
      if( val & 128 ) foo[7] = 'O';
      (void)printf("%8s", foo);
    }
    (void)printf("\n");
  }
}
#endif
