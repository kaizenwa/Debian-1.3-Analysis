/*
 * Module to dither image for monochrome display
 * uses error-diffusion dithering (floyd-steinberg)
 *                                                 
 * Adapted from dither.c module of xli by 
 * Juanita Crutchley November 1992
 *
 * the previous version of this code was written by steve losen
 * (scl@virginia.edu)
 * 
 * jim frost    07.10.89
 * Steve Losen  11.17.89
 * kirk johnson 06.04.90
 *
 ****
 * Copyright 1989, 1990 Kirk L. Johnson

 * Permission to use, copy, modify, distribute, and sell this
 * software and its documentation for any purpose is hereby granted
 * without fee, provided that the above copyright notice appear in
 * all copies and that both that copyright notice and this
 * permission notice appear in supporting documentation. The
 * author makes no representations about the suitability of this
 * software for any purpose. It is provided "as is" without express
 * or implied warranty.

 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT
 * OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *
 *****
 * Copyright 1989, 1990 Jim Frost and Steve Losen.
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
 *****
 * Copyright Juanita Crutchley and Jan Newmarch, University of Canberra 1993
 * The same conditions apply as above
 *
 */


#define MaxGrey       32768  	/* limits on the grey levels used */
#define Threshold     16384 	/* in the dithering process */
#define MinGrey       0


void
MonoFS4DitherImage (lum, cr, cb, out, h, w)
    unsigned char *lum;
    unsigned char *cr;
    unsigned char *cb;
    unsigned char *out;
    int w, h;

/*
 * simple floyd-steinberg dither with serpentine raster processing
 */

{
  unsigned int    spl;		/* source pixel length in bytes */
  unsigned int    dll;		/* destination line length in bytes */
  unsigned char  *src;		/* source data */
  unsigned char  *dst;		/* destination data */
  int		 *curr;		/* current line buffer */
  int  		 *next;		/* next line buffer */
  int	 	 *swap;		/* for swapping line buffers */
  unsigned long  intens;	/* pixel intens */
  unsigned int  level;	        /* grey level */
  unsigned int    i, j;		/* loop counters */
  int idx;                      /* loop counter */
  int error;     	        /* error in intensity level */
  int output;                   /* output intensity level */

  /*
   * dither setup
   */
  spl = 1;
  dll = (w / 8) + (w % 8 ? 1 : 0);
  src = lum;
  dst = out;

  memset(dst,0,(w*h)/8);
  curr  = (int *)calloc(1, sizeof(int)*(w+2));
  next  = (int *)calloc(1, sizeof(int)*(w+2));
  curr += 1;
  next += 1;

  /*
   * primary dither loop
   */
  for (i=0; i<h; i++)
  {
    /* copy the row into the current line */
    for (j=0; j<w; j++)
    {
      intens = (unsigned char) (*src) << 7;
      src  += spl;
      if (intens < Threshold)
    	 level = intens >> 1;
      else
         level = (((intens - Threshold) * (MaxGrey-(Threshold/2))) /
	       (MaxGrey-Threshold)) + (Threshold/2);
      curr[j] += level;
    }

    /* dither the current line */
    if (i & 0x01)
/*
 * dither a line from right to left
 */
      for (idx=(w-1); idx>=0; idx--)
      {
        output       = (curr[idx] > Threshold) ?  MaxGrey : MinGrey;
        error        = curr[idx] - output;
        curr[idx]    = output;
        next[idx+1] += error * 3 / 16;   /* diffuse errors among */
        next[idx]   += error * 5 / 16;   /* neighbouring pixels */
        next[idx-1] += error * 1 / 16;
        curr[idx-1] += error * 7 / 16;
      }     
    else
/*
 * dither a line from left to right
 */
      for (idx=0; idx<w; idx++)
      {
        output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
        error        = curr[idx] - output;
        curr[idx]    = output;
        next[idx-1] += error * 3 / 16;    /* diffuse errors among */
        next[idx]   += error * 5 / 16;    /* neighbouring pixels */
        next[idx+1] += error * 1 / 16;
        curr[idx+1] += error * 7 / 16;
      }

    /* copy the dithered line to the destination image */
    for (j=0; j<w; j++)
      if (curr[j] < Threshold)
	dst[j / 8] |= 1 << (7 - (j & 7));
    dst += dll;
    
    /* circulate the line buffers */
    swap = curr;
    curr = next;
    next = swap;
    memset(next-1,0,sizeof(int)*(w+2));
  }

  /*
   * clean up
   */
  free(curr-1);
  free(next-1);
}

