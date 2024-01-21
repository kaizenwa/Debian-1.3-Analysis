#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	disppxmp.c (Display Pixmap)
 * Purpose:	Map short buffer to picturebox display buffer
 * Subroutine:	map_buf_subzoom()		returns: void
 * Subroutine:	map_buf_repzoom()		returns: void
 * Subroutine:	map_buf_subzoom_adj()		returns: void
 * Subroutine:	map_buf_repzoom_adj()		returns: void
 * Copyright:	1987 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     31 December 1987
 *		{1} MVH Replaced zoom rep code, dumped adj stuff 21 June 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include "hfiles/coord.h"

/*
 * Subroutine:	map_buf_subzoom
 * Purpose:	Map data to a display buffer, zoomed down by sub-sampling
 */
void map_buf_subzoom ( coord, displaybuf, imagebuf, scalemap, block_sample )
     struct coordRec *coord;
     unsigned char *displaybuf;		/* i/o: pointer to display buffer */
     short *imagebuf;			/* i: pointer to image buffer */
     unsigned char *scalemap;		/* i: pointer to lookup table */
     register int block_sample;		/* i: buf to disp sub-sampling count */
{
  register short *imgb;			/* l: value pointer in image buffer */
  register unsigned char *lookup;	/* l: pointer in lookup table */
  register unsigned char *display;	/* l: pointer in display buffer */
  int disp_xwdth;			/* l: image display width in buffer */
  short disp_advance;			/* l: move disp pointer to next line */
  unsigned char *displayend;		/* l: display terminal ref point */
  short image_advance;			/* l: move img pointer to next line */

  /* initialize parameter values */
  {
    register int disp_width;		/* l: row width in display buffer */

    /* set index between starts of lines, length of scan in one output line */
    disp_width = coord->disp.width;
    disp_xwdth = coord->bd.dstXwdth;
    display = displaybuf + ((coord->bd.dstY1 * disp_width) + coord->bd.dstX1);
    imgb = imagebuf +
      ((coord->bd.srcY1 * coord->buf.width) + coord->bd.srcX1);
    lookup = scalemap;
    /* next line minus already ticked off (zoom * bwdth) - (zoom * dwdth) */
    image_advance = (coord->buf.width - disp_xwdth) * block_sample;
    disp_advance = disp_width - disp_xwdth; 
    displayend = display + (coord->bd.dstYhght * disp_width);
  }
  /* loop through sub-sampling */
  {
    register unsigned char *displimit;

    while( display < displayend ) {
      /* make a sub-sampled line */
      displimit = display + disp_xwdth;
      while( display < displimit ) {
	*display++ = lookup[*imgb];
	imgb += block_sample;
      }
      display += disp_advance;
      imgb += image_advance;
    }
  }
}

/*
 * Subroutine:	map_buf_repzoom
 * Purpose:	Put zoom replicated image data in the display buffer
 */
#ifdef ANSIC
void map_buf_repzoom ( struct coordRec *coord, unsigned char *dst_buf,
		       short *src_buf, unsigned char *scalemap )
#else
void map_buf_repzoom ( coord, dst_buf, src_buf, scalemap )
     struct coordRec *coord;
     unsigned char *dst_buf;		/* i/o: pointer to display buffer */
     short *src_buf;			/* i: pointer to image buffer */
     unsigned char *scalemap;		/* i: pointer to lookup table */
#endif
{
  register int zoom;			/* l: 1D pixel replication count */
  register unsigned char *dst;		/* l: pointer in display buffer */
  register unsigned char val;		/* l: value mapped from lookup table */
  register unsigned char* xrep_limit;
  register unsigned char *lookup;	/* l: pointer in lookup table */
  register short *src;			/* l: value pointer in image buffer */
  short *src_line;			/* l: line start in image buffer */
  unsigned char* dst_x_limit;
  unsigned char *dst_line;		/* l: start line in display buffer */
  int y_dst_count;
  int x_dst_count;
  int dst_x, dst_y;
  int dst_width, dst_height;
  int src_x, src_y;
  int src_width, src_height;
  int yrep_limit;
  int first_xrep;

  /*  Set basic parameters  */
  zoom = -coord->bd.block;
  dst_x = coord->bd.dst_x;
  dst_y = coord->bd.dst_y;
  dst_width = coord->disp.width;
  dst_height = coord->disp.height;
  src_width = coord->buf.width;
  src_height = coord->buf.height;
  /*  If buffer extends to or beyond lower edge  */
  y_dst_count = dst_y + (src_height * zoom);
  if( y_dst_count > dst_height )
    y_dst_count = dst_height;
  if( dst_y <= 0 ) {
    /*  Compute starting offset in line, and first repetition count  */
    src_y = -dst_y / zoom;
    yrep_limit = dst_y + ((src_y + 1) * zoom);
    dst_y = 0;
  } else if( dst_y < dst_height ) {
    src_y = 0;
    yrep_limit = zoom;
    y_dst_count -= dst_y;
  } else
    return;
   
  /*  If buffer extends to or beyond left edge  */
  x_dst_count = dst_x + (src_width * zoom);
  if( x_dst_count > dst_width )
    x_dst_count = dst_width;
  if( dst_x <= 0 ) {
    /*  Compute starting offset in line, and first repetition count  */
    src_x = -dst_x / zoom;
    first_xrep = dst_x + ((src_x + 1) * zoom);
    dst_x = 0;
  } else if( dst_x < dst_width ) {
    src_x = 0;
    first_xrep = zoom;
    x_dst_count -= dst_x;
  } else
    return;

  lookup = scalemap;
  /*  Start pointers for display_data, and image_data  */
  dst_line = dst_buf + ((dst_y * dst_width) + dst_x);
  src_line = src_buf + ((src_y * src_width) + src_x);
  dst_y = 0;

  {
    register unsigned char *xreplimit;		/* loop comparison pointer */
    register unsigned char *displaylimit;	/* limit comparison pointer */

    /*  Loop by image row  */
    while( dst_y < y_dst_count ) {
      /*  Update src and dst pointers  */
      src = src_line;
      dst = dst_line;
      dst_x_limit = dst + x_dst_count;
      /*  First pixel may be replicated less  */
      xrep_limit = dst + first_xrep;
      while( xrep_limit < dst_x_limit ) {
	/* get output val */
	val = lookup[*src++];
	/*  Replicate  */
	while( dst < xrep_limit ) {
	  *dst++ = val;
	}
	/*  Reset replication counter  */
	xrep_limit += zoom;
      }
      /*  Repeat for last val (replicate to end)  */
      val = lookup[*src++];
      while( dst < dst_x_limit ) {
	*dst++ = val;
      }
      /*  Get pointers to next line */
      dst = dst_line;
      dst_line += dst_width;
      dst_y++;
      /*  Make sure line copying doesn't go too far  */
      if( yrep_limit > y_dst_count )
      	yrep_limit = y_dst_count;
      /*  Replicate rows  */
      while( dst_y < yrep_limit ) {
#ifdef ANSIC
	memcopy((char *)dst_line, (char *)dst, x_dst_count);
#else
	bcopy((char *)dst, (char *)dst_line, x_dst_count);
#endif ANSIC
	dst_line += dst_width;
	dst_y++;
      }
      /*  Set up next line replication counter  */
      yrep_limit += zoom;
      /*  Advance image pointer to place in next line */
      src = (src_line += src_width);
    }
  }
}


