#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdtemp.c (Coordinate Temporary)
 * Purpose:	Calculate display coordinates for pan and zoom
 * Subroutine:	set_tdisp()			returns: void
 * Subroutine:	reset_tdisp()			returns: void
 * Subroutine:	set_disptran()			returns: void
 * Subroutine:	panedge_zoom()			returns: void
 * Subroutine:	set_dispoff()			returns: void
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     1 September 1988
 *		{1} MVH Kludged set_dispoff to trap overrrun	28 April 1990
 *		{2} Doug Mink add argument to set_transform	26 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc */
#include "hfiles/coord.h"	/* coord structs */

#define MAX(a,b) (((a) > (b)) ? (a) : (b))

/*
 * Subroutine:	set_tdisp
 * Purpose:	set needed parameters for proposed display
 * PreState:	tid.cenX, tid.cenY, tid.zoom, and tid.block must be set
 */
void set_tdisp ( coord )
     struct coordRec *coord;
{
  /* calculate corners of display in img coords */
  /* 0.5 accounts for rounding to nearest pixel, 0.499 rounding at limit */
  coord->tid.srcX1 = (int)
    (coord->tid.cenX - ((coord->disp.cenX - 0.5) / coord->tid.zoom));
  coord->tid.srcX2 = (int)
    (coord->tid.cenX +
     ((0.499 + (float)coord->disp.width - coord->disp.cenX) /
      coord->tid.zoom));
  coord->tid.srcY1 = (int)
    (coord->tid.cenY - ((coord->disp.cenY - 0.5) / coord->tid.zoom));
  coord->tid.srcY2 = (int)
    (coord->tid.cenY +
     ((0.499 + (float)coord->disp.height - coord->disp.cenY) /
      coord->tid.zoom));
  /* limit display area to img area */
  coord->tid.clip = 0;
  if( coord->tid.srcX1 < coord->img.X1i ) {
    coord->tid.srcX1 = coord->img.X1i;
    coord->tid.clip += LX;
  }
  if( coord->tid.srcX2 > coord->img.X2i ) {
    coord->tid.srcX2 = coord->img.X2i;
    coord->tid.clip += RX;
  }
  if( coord->tid.srcY1 < coord->img.Y1i ) {
    coord->tid.srcY1 = coord->img.Y1i;
    coord->tid.clip += TY;
  }
  if( coord->tid.srcY2 > coord->img.Y2i ) {
    coord->tid.srcY2 = coord->img.Y2i;
    coord->tid.clip += BY;
  }
  /* check display parameters against buf contents */
  if( coord->bufcheck ) {
    /* request outside coverage of buffer contents */
    if( (coord->tid.srcX1 < coord->ib.srcX1) ||
        (coord->tid.srcX2 > coord->ib.srcX2) ||
	(coord->tid.srcY1 < coord->ib.srcY1) ||
        (coord->tid.srcY2 > coord->ib.srcY2) )
      coord->buferror = 1;
    else if( coord->ib.block != 1 ) {
      if( coord->tid.block < coord->ib.block )
	/* request for finer resolution than currently in buffer */
	coord->buferror = 1;
      else if( ((int)coord->tid.block % coord->ib.block) != 0 )
	/* request for incompatible blocking */
	coord->buferror = 1;
      else
	coord->buferror = 0;
    } else
      coord->buferror = 0;
  }
}

/*
 * Subroutine:	reset_tdisp
 * Purpose:	Reset temp disp proposal to the current disp params
 */
void reset_tdisp ( coord )
     struct coordRec *coord;
{
  bcopy((char *)&coord->id, (char *)&coord->tid, sizeof(Edges));
  coord->buferror = 0;
}

/*
 * Subroutine:	set_disptran
 * Purpose:	Set display parameters and translations as per temp
 */
void set_disptran ( coord )
     struct coordRec *coord;
{
  void set_transform(), combine_transform(), invert_transform();

  bcopy((char *)&coord->tid, (char *)&coord->id, sizeof(Edges));
  /* compute coordinate conversions between pan and img systems */
  set_transform(coord->id.cenX, coord->id.cenY, coord->id.zoom,
		coord->id.zoom, &coord->disp, &coord->img,
		&coord->disptoimg, &coord->imgtodisp);
  /* compute transform from display to file and the reverse */
  combine_transform(&coord->disptofile,
		    &coord->disptoimg, &coord->imgtofile);
  invert_transform(&coord->filetodisp, &coord->disptofile,
		   (double)coord->file.ioff);
}

/*
 * Subroutine:	panedge_zoom
 * Purpose:	set up zoom given edges of desired display and
 *		appropriate img transform
 */
void panedge_zoom ( coord, wintoimgtrans, win_x, win_y )
     struct coordRec *coord;	/* i: collected coords */
     Transform *wintoimgtrans;	/* i: transform from mouse's window to img */
     int win_x, win_y;		/* i: window coords of mouse event */
{
  float imgX, imgY;
  int box_width, box_height;
  void i_transform();
  static int choose_zoom();

  /* calculate image coordinates of win_x and win_y */
  i_transform(wintoimgtrans, win_x, win_y, &imgX, &imgY);
  /* select zoom based on this box */
  box_width = abs((int)coord->tid.cenX - (int)imgX) * 2 + 1;
  box_height = abs((int)coord->tid.cenY - (int)imgY) * 2 + 1;
  coord->tid.block = choose_zoom(coord->disp.width, coord->disp.height,
				 box_width, box_height);
  /* check against already generous limits and set float zoom factor */
  if( coord->tid.block > 0 ) {
    if( coord->tid.block > 400 )
      coord->tid.block = 400;
    coord->tid.zoom = 1.0 / (double)coord->tid.block;
  } else {
    if( coord->tid.block < -100 )
      coord->tid.block = -100;
    coord->tid.zoom = -(double)coord->tid.block;
  }
}

/*
 * Subroutine:	choose_zoom
 * Purpose:	Select the zoom to best fit indicated image subsection to
 *		display window
 * Method:	Choose a zoom which comes closest to putting the ref point
 *		on an edge while still enclosing it
 * Returns:	Source-to-dest integer blocking code
 */
 static int choose_zoom ( dst_width, dst_height, src_width, src_height )
     int dst_width;	/* width of window */
     int dst_height;	/* height of window */
     int src_width;	/* width of image section */
     int src_height;	/* height of image section */
{
  int i;

  /* limit width and height just in case */
  src_width = MAX(1, src_width);
  src_height = MAX(1, src_height);
#ifdef DEBUG
  if( (dst_width <= 0) || (dst_height <= 0) ) {
    (void)fprintf(stderr, "WARNING: no window size for choose_zoom\n");
    return(1.0);
  }
#endif
  /* will we zoom up or down */
  /* if source image section has more pixels than destination, block */
  if( (src_width > dst_width) || (src_height > dst_height) ) {
    /* point is outside zoom-1 frame, enlarge source until we include it */
    for( i = 1;
	(((dst_width * i) <= src_width) || ((dst_height * i) <= src_height));
	i++ );
    return( i );
  } else {
    /* point is inside zoom-one frame */
    /* try shrinking source until we no-longer include it, then back-up one */
    /* (count until we reach a point where one error changes sign) */
    for( i = 1;
	(((src_width * i) <= dst_width) &&
	 ((src_height * i) <= dst_height));
	i++ );
    if( --i == 1 ) return(1);
    else return( -i );
  }
}

/*
 * Subroutine:	set_dispoff
 * Purpose:	Set the special window (dest) coords for mapping a display
 * Note:	Round pixel index in if on an edge between pixels
 * Note:	A is src (buf), B is dst (disp)
 */
void set_dispoff ( BAtrans, Bsys, AB )
     Transform *BAtrans;
     Coordsys *Bsys;
     Edges *AB;
{
  float x, y;
  int BAblock;

  /*  Compute simple coords for simplified display routines  */
  x = -BAtrans->add_outx / BAtrans->inx_outx;
  if( x < 0 )
    AB->dst_x = (int)(x - 0.5);
  else
    AB->dst_x = (int)(x + 0.5);
  y = -BAtrans->add_outy / BAtrans->iny_outy;
  if( y < 0 )
    AB->dst_y = (int)(y - 0.5);
  else
    AB->dst_y = (int)(y + 0.5);
  d_transform(BAtrans, 0.0, 0.0, &x, &y);
  if( x < 0 )
    AB->src_x = (int)(x - 0.5);
  else
    AB->src_x = (int)(x + 0.5);
  if( x < 0 )
    AB->src_y = (int)(y - 0.5);
  else
    AB->src_y = (int)(y + 0.5);
  /*  Compute [not completely correctly] paramaters for old display code  */
  BAblock = -AB->block;
  /* do coords for top-left corner */
  if( (BAblock > 0) || ((AB->clip & LXTY) != 0) ) {
    /* calculate upperleft edge backwards through transform */
    x = ((float)AB->srcX1 - BAtrans->add_outx) / BAtrans->inx_outx;
    y = ((float)AB->srcY1 - BAtrans->add_outy) / BAtrans->iny_outy;
    AB->dstX1 = (int)(x + 0.001);
    AB->dstY1 = (int)(y + 0.001);
    if( AB->dstX1 < Bsys->X1i )
      AB->dstX1 = Bsys->X1i;
    if( AB->dstY1 < Bsys->Y1i )
      AB->dstY1 = Bsys->Y1i;
  } else {
    /* block 1 or less */
    AB->dstX1 = Bsys->X1;
    AB->dstY1 = Bsys->Y1;
  }
  if( (BAblock > 0) || ((AB->clip & RXBY) != 0) ) {
    /* calculate lowerright edge backwards through transform */
    x = ((float)(AB->srcX2 + 1) - BAtrans->add_outx) / BAtrans->inx_outx;
    y = ((float)(AB->srcY2 + 1) - BAtrans->add_outy) / BAtrans->iny_outy;
    AB->dstX2 = (int)(x - 0.001);
    AB->dstY2 = (int)(y - 0.001);
    if( AB->dstX2 > Bsys->X2i )
      AB->dstX2 = Bsys->X2i;
    if( AB->dstY2 > Bsys->Y2i )
      AB->dstY2 = Bsys->Y2i;
  } else {
    AB->dstX2 = Bsys->X2;
    AB->dstY2 = Bsys->Y2;
  }
  AB->dstXwdth = 1 + AB->dstX2 - AB->dstX1;
  AB->dstYhght = 1 + AB->dstY2 - AB->dstY1;
  /* kludge trap to prevent algorithm from overshooting image buffer */
  if( (BAtrans->inx_outx >= 1.0) &&
      ((AB->dstXwdth * BAtrans->inx_outx) > AB->srcXwdth) ) {
    AB->dstX2--;
    AB->dstXwdth--;
  }
  if( (BAtrans->iny_outy >= 1.0) &&
      ((AB->dstYhght * BAtrans->iny_outy) > AB->srcYhght) ) {
    AB->dstY2--;
    AB->dstYhght--;
  }
}
