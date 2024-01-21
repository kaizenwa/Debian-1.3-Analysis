/* dither.c:
 *
 * routine for dithering a color image to monochrome based on color
 * intensity.  this is loosely based on an algorithm which barry shein
 * (bzs@std.com) used in his "xf" program.
 *
 * jim frost 07.10.89
 *
 *****
 * Copyright 1989, 1990, 1991 Jim Frost
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  The author makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * 
 * Modified by Juanita Crutchley for use in mpeg-1.0
 * November 1992
 *
 * Copyright Juanita Crutchley and Jan Newmarch, University of Canberra, 1993
 * The same copyright conditions apply as above.
 */


/* 4x4 arrays used for dithering, arranged by nybble
 */

#define GRAYS    17 /* ((4 * 4) + 1) patterns for a good dither */
#define GRAYSTEP ((unsigned long)(65536 / GRAYS))

static unsigned char  DitherBits[GRAYS][4] = {
  0xf, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xa, 0xf,
  0xa, 0xd, 0xa, 0xf,
  0xa, 0xd, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x5,
  0x8, 0x5, 0xa, 0x5,
  0x8, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x0
};

/*
 *--------------------------------------------------------------
 *
 * HalftoneDitherImage --
 *
 * 	simple dithering algorithm, really optimized for the 4x4 array
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
void
HalftoneDitherImage (lum, cr, cb, out, h, w)
    unsigned char *lum;
    unsigned char *cr;
    unsigned char *cb;
    unsigned char *out;
    int w, h;
{
    int width, height;		  /* width and height of new "blown up" image */
    unsigned char *sp, *dp, *dp2; /* data pointers */
    unsigned int   dindex;        /* index into dither array */
    unsigned int   spl;           /* source pixel length in bytes */
    unsigned int   dll;           /* destination line length in bytes */
    unsigned long  color;         /* pixel color */
    unsigned int   a, x, y;       /* random counters */

  /* set up
   */

    width = w*4;
    height = h*4;
    spl= 1;	/* num bits per pixel */
    dll= (width / 8) + (width % 8 ? 1 : 0);

    /* dither each pixel
     */
  
    sp= lum;
    dp= out;
    memset(dp,'\0',(width*height)/8);  /* initialise byte stream */
    for (y= 0; y < h; y++) {
        for (x= 0; x < w; x++) {
          dp2= dp + (x >> 1);
          color= (unsigned char) *sp;
          dindex= (unsigned long) (color<<8)/GRAYSTEP;
          if (dindex >= GRAYS) /* rounding errors can do this */
		dindex= GRAYS - 1;
  
        /* loop for the four Y bits in the dither pattern, putting all
         * four X bits in at once.  if you think this would be hard to
         * change to be an NxN dithering array, you're right, since we're
         * banking on the fact that we need only shift the mask based on
         * whether x is odd or not.  an 8x8 array wouldn't even need that,
         * but blowing an image up by 64x is probably not a feature.
         */
  
          if (x & 1)
	      for (a= 0; a < 4; a++, dp2 += dll) 
		  *dp2 |= DitherBits[dindex][a];
          else
	      for (a= 0; a < 4; a++, dp2 += dll)
		  *dp2 |= (DitherBits[dindex][a] << 4);
          sp += spl;
        } /* end x for */
        dp += (dll << 2); /* (dll * 4) but I like shifts */
    } /* end y for */
  
}
