#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	MakeBorder.c
 * Purpose:	Create bitmap with border pattern for drawing button.
 * Subroutines:	btn_MakeBdrBitmap()		returns: void
 * Subroutines:	static btn_ReverseByte()	returns: unsigned char
 * Subroutines:	static RightBorder()		returns: void
 * Xlib calls:	none
 * Copyright:	1987, 1989, 1990, 1995 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     31 December 1987
 *		{1} MVH	X11 version, chars instead of shorts	16 March 1989
 *		{2} MVH form defines all 4 corners uniquely	26 March 1990
 *		{3} Doug Mink   cast before shifting right	   4 May 1995
 *		{n} <who> -- <does what> -- <when>
 */

/*
 * Subroutine:	btn_MakeBdrBitmap
 * Purpose:	Fill a bitmap of an empty button with a border pattern made
 *		from the given form.
 * Returns:	void
 * Called by:	btn_LabelButtons() in MakeBox.c
 * Uses:	btn_ReverseByte(), btn_RightBorder() below
 * Xlib calls:	none
 * Post-state:	bitmap buffer allocated and filled
 * Method:	Creates rectangular button bitmap (no symmetry assumed).  The
 *		pattern form is 32x32 with all 4 edges of button.
 * Note:	For each byte, bit 0 (0x01) appears on the left.
 * Note:	For each byte, bit 7 (0x80) appears on the right.
 */
void btn_MakeBdrBitmap ( buttonmap, width, height, byte_width, form, inverse )
     unsigned char *buttonmap;	/* i,o: pointer to bitmap of button */
     int width, height;		/* i: dimensions of button window (to cover) */
     int byte_width;		/* i: bytes in one line or row */
     unsigned char *form;	/* i: ptr to pattern form for entier button */
     int inverse;		/* i: flag to invert bits (reverse video) */
{
  unsigned char *top_line;	/* l: ptr to line from top in buttonmap */
  unsigned char *bottom_line;	/* l: ptr to line from bottom in buttonmap */
  unsigned char *top_form;	/* l: ptr to current top row in form */
  unsigned char *bottom_form;	/* l: ptr to current bottom row in form */
  int form_rows;		/* l: number of rows of form to use */
  int row;			/* l: loop counter */
  int right_form_byte;		/* l: index of first right form byte to use */
  int right_form_bit;		/* l: bit in first right form byte to use */
  int right_bdr_byte;		/* l: index of first line byte for right bdr */
  int right_bdr_bit;		/* l: bit in first line byte for right bdr */
  static unsigned char btn_ReverseByte();
  static void btn_MakeBdrLine();

  /* portion of pattern form used (clip overlap if button very small) */
  /* middle overlap on odd size counted for height, omitted for width */
  if( height >= 32 )
    form_rows = 16;
  else
    form_rows = (height + 1) / 2;
  /* buttonmap byte index for first byte of top and bottom lines */
  top_line = buttonmap;
  bottom_line = &buttonmap[(height - 1) * byte_width];
  /* normal starting point in reverse form for right border work */
  right_form_byte = 0;
  right_form_bit = 0;
  /* determine parameters form making right border */
  if( width >= 32 ) {
    right_bdr_byte = (width - 16) / 8;
    right_bdr_bit = width & 0x0007;
  } else if( width == 31 ) {
    right_bdr_byte = 2;
    right_bdr_bit = 0;
    right_form_bit = 1;
  } else {
    if( width > 14 ) {
      right_bdr_byte = 1;
      right_bdr_bit = (width - 15) / 2;
    } else {
      right_bdr_byte = 0;
      right_bdr_bit = (width + 1) / 2;
    }
    if( width > 17 ) {
      right_form_bit = 16 - (width / 2);
    } else {
      right_form_byte = 1;
      right_form_bit = 8 - (width / 2);
    }
  }
  top_form = form;
  /* bottom line of 4 x 32 byte form */
  bottom_form = form + 124;
  /* do top and bottom border patterns */
  for( row=0; row<form_rows; row++ ) {
    btn_MakeBdrLine(top_line, top_form, byte_width, inverse, right_bdr_byte,
		    right_bdr_bit, right_form_byte, right_form_bit);
    if( top_line != bottom_line )
      btn_MakeBdrLine(bottom_line, bottom_form, byte_width, inverse,
		      right_bdr_byte, right_bdr_bit,
		      right_form_byte, right_form_bit);
    top_form += 4;
    bottom_form -= 4;
    top_line += byte_width;
    bottom_line -= byte_width;
  }
  /* working from where we left at bottom, up to where we left at top */
  top_line -= byte_width;
  while( top_line < bottom_line ) {
    /* duplicate the lowest line of the upper pattern */
    bcopy((char *)top_line, (char *)bottom_line, byte_width);
    bottom_line -= byte_width;
  }
}

/*
 * Subroutine:	btn_MakeBdrLine
 * Purpose:	set the bits for one line of the button's border
 */
static void btn_MakeBdrLine ( line, form, byte_width, inverse,
			      right_bdr_byte, right_bdr_bit,
			      right_form_byte, right_form_bit )
     unsigned char *line;	/* l: ptr to line from top in buttonmap */
     unsigned char *form;	/* l: ptr to current top row in form */
     int byte_width;		/* i: bytes in one line or row */
     int inverse;		/* i: flag to invert bits (reverse video) */
     int right_form_byte;	/* l: index of first right form byte to use */
     int right_form_bit;	/* l: bit in first right form byte to use */
     int right_bdr_byte;	/* l: index of first line byte for right bdr */
     int right_bdr_bit;		/* l: bit in first line byte for right bdr */
{
  static void btn_RightBorder();
  /* copy in top left, store reverse for top right */
  *line = *form;
  *(line+1) = *(++form);
  /* fill middle across the row */
  if( *form & 0x80 ) {
    int i;	/* l: byte index offset and loop counter */
    for( i=2; i<=right_bdr_byte; i++ )
      line[i] = 0xff;
  }
  /* else not needed since btn_Alloc cleared all bits anyway */
  /* would be - bzero((char *)(top_line + 2, right_bdr_byte - 1); */
  /* do the right side using 3rd and 4th byte in form row */
  btn_RightBorder((++form), line + right_bdr_byte,
		  right_form_byte, right_form_bit, right_bdr_bit);
  if( inverse ) {
    int i;
    for( i=0; i<byte_width; i++ )
      line[i] = 0xff - line[i];
  }
}

#ifdef UNNEEDED
/*
 * Subroutine:	btn_ReverseByte
 * Purpose:	Get byte with bits in reverse order of that given
 * Returns:	Unsigned byte which is mirror copy of byte given
 * Called by:	btn_MakeBdrBitmap() above
 * Uses:	flip[] below
 * Xlib calls:	none
 * Method:	Does lookup on 4bit pattern, using index of original 4bit value
 */
static unsigned char flip[16] = {
  0x0, 0x8, 0x4, 0xc, 0x2, 0xa, 0x6, 0xe,
  0x1, 0x9, 0x5, 0xd, 0x3, 0xb, 0x7, 0xf
};
static unsigned char btn_ReverseByte ( byte )
     unsigned char byte;
{
  unsigned char reverse;

  reverse = flip[byte >> 4];
  reverse += flip[byte & 0xf] << 4;
  return( reverse );
}
#endif

/*
 * Subroutine:	btn_RightBorder
 * Purpose:	Set the bits for the right border pattern
 * Returns:	void
 * Called by:	btn_MakeBdrBitmap() above
 * Uses:	rmask[] and lmask[] below
 * Xlib calls:	none
 * Post-state:	bitmap buffer line right border bits set
 * Method:	Given 2 byte pattern, byte and bit numbers, and bimtap byte
 *		pointer and bit number, copy to end of 2nd pattern byte.
 * Note:	bit 0 (0x01) appears to the left of bit 7 (0x80).
 */
/* mask of ones for bits included at index and to the right */
static unsigned char rmask[8] = { 0xff,0xfe,0xfc,0xf8,0xf0,0xe0,0xc0,0x80 };
/* mask of ones for bits included at index and to the left */
static unsigned char lmask[8] = { 0x00,0x01,0x03,0x07,0x0f,0x1f,0x3f,0x7f };
static void btn_RightBorder ( src, dst, src_byte, src_bit, dst_bit )
     unsigned char *src;	/* i: ptr to two reversed form pattern bytes */
     unsigned char *dst;	/* i: ptr to first map byte affected */
     int src_byte;		/* i: index of first form pattern byte used */
     int src_bit;		/* i: bit in first source byte (7 on left) */
     int dst_bit;		/* i: bit in dst (7 on left) */
{
  /* if there is perfect allignment, easy work and no shift */
  if( src_bit == dst_bit ) {
    *dst = (*dst & lmask[dst_bit]) | (src[src_byte] & rmask[src_bit]);
    if( src_byte < 1 )
      dst[1] = src[1];
    return;
  }
  /* do segment by segment, advancing apropriate byte each time */
  do {
    if( dst_bit > src_bit ) {
      *dst = (*dst & lmask[dst_bit]) |
	((src[src_byte] & rmask[src_bit]) << (dst_bit - src_bit));
      src_bit += (8-dst_bit);
      dst_bit = 0;
      dst++;
    } else {
      *dst = (*dst & lmask[dst_bit]) |
	((unsigned char)(src[src_byte] & rmask[src_bit]) >> (unsigned int)(src_bit - dst_bit));
      dst_bit += (8-src_bit);
      src_bit = 0;
      src_byte++;
    }
  } while( src_byte < 2 );
}
