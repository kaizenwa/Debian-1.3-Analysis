#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	crdset.c (Coordinate Set)
 * Purpose:	Set up coordinate system parameters and translation matrices
 * Subroutine:	set_coordsys()		returns: void
 * Subroutine:	set_transform()		returns: void
 * Subroutine:	set_imgtran()		returns: void
 * Subroutine:	set_fbuftran()		returns: void
 * Subroutine:	set_buftran()		returns: void
 * Subroutine:	set_edges()		returns: void
 * Xlib calls:	none
 * Notes:
 *  the base coordinate system is the img
 *  coordinate conversions are computed by passing through the img
 *  the img has 0.0, 0.0 as its upper left corner
 *  files often have 0.5,0.5 at lower left corner (index from 1, ioff = 0.0)
 *  disp, buffer, and pan have 0.0,0.0 in upper left corner (ioff = 0.5)
 *  integer values refer to the center of the pixel in its coordinate system
 *  translations always produce a real (float) value output
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     1 September 1988
 *		{1} MVH support for file buffer coord sys	  19 Feb 1990
 *		{2} Doug Mink different X,Y zoom factors	  26 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include "hfiles/coord.h"		/* coord structs */
#include "hfiles/image.h"		/* image struct */

/*
 * Subroutine:	set_coordsys
 * Purpose:	Set basic params for a coordinate system
 */
void set_coordsys ( csys, ioff, width, height, x1, y1, x2, y2 )
     Coordsys *csys;
     double ioff;
     int width, height;
     int x1, y1, x2, y2;
{
  csys->width = width;
  csys->height = height;
  csys->ioff = ioff;
  /* pixel coords of four sides (col or row just inside the edge) */
  csys->X1i = x1;
  csys->Y1i = y1;
  csys->X2i = x2;
  csys->Y2i = y2;
  csys->Xwdth = 1 + x2 - x1;
  csys->Yhght = 1 + y2 - y1;
  /* put the center in the center of the pixel before the true center */
  csys->cenX = (float)(x1 + ((csys->Xwdth - 1) / 2)) + ioff;
  csys->cenY = (float)(y1 + ((csys->Yhght - 1) / 2)) + ioff;
  /* determine real coords of outer edge using ... */
  /* ... offset from integer coords to real coords */
  if( ioff < 0.25 ) {
    csys->X1 = (float)x1 - (float)0.5;
    csys->Y1 = (float)y1 - (float)0.5;
    csys->X2 = (float)x2 + (float)0.5;
    csys->Y2 = (float)y2 + (float)0.5;
  } else {
    csys->X1 = (float)x1;
    csys->Y1 = (float)y1;
    csys->X2 = (float)(x2 + 1);
    csys->Y2 = (float)(y2 + 1);
  }
}

/*
 * Subroutine:	set_transform
 * Purpose:	Calculate transforms given where to put A on B
 * Note:	Forward transform is A to B
 */
void set_transform ( BAcenX, BAcenY, BAzoomX, BAzoomY, Asys, Bsys, AtoB, BtoA )
     double BAcenX, BAcenY;	/* i: B coords of center of A system */
     double BAzoomX,BAzoomY;	/* i: size of a B pixel in A units */
     Coordsys *Asys, *Bsys;
     Transform *AtoB, *BtoA;
{
  float ABzoom;
  void set_trans_speed(), invert_transform();

  ABzoom = 1.0 / BAzoomX;
  AtoB->inx_outx = ABzoom;
  AtoB->iny_outx = 0.0;
  AtoB->add_outx = BAcenX - (Asys->cenX * ABzoom);
  /* if given an integer parameter, its float value would be less by ioff */
  AtoB->iadd_outx = BAcenX - ((Asys->cenX - Asys->ioff) * ABzoom);
  AtoB->inx_outy = 0.0;
  ABzoom = 1.0 / BAzoomY;
  AtoB->iny_outy = ABzoom;
  AtoB->add_outy = BAcenY - (Asys->cenY * ABzoom);
  AtoB->iadd_outy = BAcenY - ((Asys->cenY - Asys->ioff) * ABzoom);
  /* compute speedy integer computation parameters */
  set_trans_speed(AtoB);
  /* compute the inverse parameters (complete) */
  invert_transform(BtoA, AtoB, Bsys->ioff); 
}

/*
 * Subroutine:	set_imgtran
 * Purpose:	Set the img-to-file transform
 */
void set_imgtran ( image, img, file, imgtofile, filetoimg )
     struct imageRec *image;
     Coordsys *img, *file;
     Transform *imgtofile, *filetoimg;
{
  float if_zoom;
  void rotate_transform(), set_trans_speed(), invert_transform();

  if_zoom = 1.0 / image->fiblock;
  imgtofile->inx_outx = if_zoom;
  imgtofile->iny_outx = 0.0;
  imgtofile->add_outx = image->ficenX - (img->cenX * if_zoom);
  /* if given an integer parameter, its float value would be less by ioff */
  imgtofile->iadd_outx = image->ficenX - ((img->cenX - img->ioff) * if_zoom);
  imgtofile->inx_outy = 0.0;
  imgtofile->iny_outy = if_zoom;
  imgtofile->add_outy = image->ficenY - (img->cenY * if_zoom);
  imgtofile->iadd_outy = image->ficenY - ((img->cenY - img->ioff) * if_zoom);
  /* if rotation or y coordinate flipping is to be applied, do it new */
  if( (image->rotate_code != 0) || (image->row_order != 0) ) {
    rotate_transform(img, imgtofile, image->row_order, image->rotate_code);
  }
  /* compute speedy integer computation parameters */
  set_trans_speed(imgtofile);
  /* compute the inverse parameters (complete) */
  invert_transform(filetoimg, imgtofile, file->ioff);
}

/*
 * Subroutine:	set_fbuftran
 * Purpose:	Set buffer-to-filebuffer transform, should the
 *		two buffers differ.
 * Note:	Two buffers have same dimensions (and are usually the same)
 */
void set_fbuftran ( image, buf, fbuf, buftofbuf )
     struct imageRec *image;
     Coordsys *buf, *fbuf;	/* descriptions of short and file buffers */
     Transform *buftofbuf;
{
  void rotate_transform(), set_trans_speed();

  buftofbuf->inx_outx = 1.0;
  buftofbuf->iny_outx = 0.0;
  buftofbuf->add_outx = 0.0;
  buftofbuf->iadd_outx = 0.5;
  buftofbuf->inx_outy = 0.0;
  buftofbuf->iny_outy = 1.0;
  buftofbuf->add_outy = 0.0;
  buftofbuf->iadd_outy = 0.5;
  /* if rotation or y coordinate flipping is to be applied, do it new */
  if( (image->rotate_code != 0) || (image->row_order != 0) ) {
    rotate_transform(buf, buftofbuf, image->row_order, image->rotate_code);
  }
  /* compute speedy integer computation parameters */
  set_trans_speed(buftofbuf);
  /* set the fbuf coordinate system parameters */
  if( image->rotate_code & 1 ) {
    /* rotated 90 or 270 degrees means switch X and Y axes */
    fbuf->X1 = buf->Y1;
    fbuf->X2 = buf->Y2;
    fbuf->Y1 = buf->X1;
    fbuf->Y2 = buf->X2;
    fbuf->X1i = buf->Y1i;
    fbuf->X2i = buf->Y2i;
    fbuf->Y1i = buf->X1i;
    fbuf->Y2i = buf->X2i;
    fbuf->Xwdth = buf->Yhght;
    fbuf->Yhght = buf->Xwdth;
    fbuf->cenX = buf->cenY;
    fbuf->cenY = buf->cenX;
    fbuf->width = buf->height;
    fbuf->height = buf->width;
    fbuf->ioff = buf->ioff;
  } else
    bcopy((char *)buf, (char *)fbuf, sizeof(Coordsys));
}

/*
 * Subroutine:	set_buftran
 * Purpose:	Set buffer-to-img transform (& buf-to-file)
 * Note:	adjusts alignment so as not to waste buffer space
 * Note:	edges later used to check whether requested display is
 *		available
 * Exception:	assumes buf is not smaller than disp
 */
void set_buftran ( coord )
     struct coordRec *coord;
{
  void set_transform(), combine_transform(), set_edges(), invert_transform();

  /* if buf and image are identical, just copy */
  if( !coord->bufcheck ) {
    set_transform(coord->img.cenX, coord->img.cenY, 1.0, 1.0,
		  &coord->buf, &coord->img,
		  &coord->buftoimg, &coord->imgtobuf);
    bcopy((char *)&(coord->imgtofile),
	  (char *)&(coord->buftofile), sizeof(Transform));
    coord->ib.srcX1 = 0;
    coord->ib.srcX2 = coord->img.width - 1;
    coord->ib.srcY1 = 0;
    coord->ib.srcY2 = coord->img.height - 1;
    coord->ib.srcXwdth = coord->img.width;
    coord->ib.srcYhght = coord->img.height;
    coord->ib.cenX = coord->img.cenX;
    coord->ib.cenY = coord->img.cenY;
    coord->ib.zoom = 1.0;
    coord->ib.block = 1;
  } else {
    float lX, tY, rX, bY, diff;
    {
      register double ibzoom;

      /* if the buffer needs checking, calculate buffer to img zoom */
      if( (coord->buf.width < ((double)coord->disp.width /
			       coord->id.zoom)) ||
	 (coord->buf.height < ((double)coord->disp.height /
			       coord->id.zoom)) ) {
	/* use idzoom if zoom 1 won't work (check disp size in img coords) */
	ibzoom = coord->id.zoom;
	coord->ib.block = coord->id.block;
      } else {
	ibzoom = 1.0;
	coord->ib.block = 1;
      }
      coord->ib.zoom = ibzoom;
      /* calculate corners of display in img coords */
      coord->ib.cenX = coord->id.cenX;
      coord->ib.cenY = coord->id.cenY;
      lX = coord->ib.cenX - (coord->buf.cenX * ibzoom);
      rX = coord->ib.cenX +
	(((double)coord->buf.width - coord->buf.cenX) * ibzoom);
      tY = coord->ib.cenY - (coord->buf.cenY * ibzoom);
      bY = coord->ib.cenY +
	(((double)coord->buf.height - coord->buf.cenY) * ibzoom);
    }
    /* shift center if buffer extends beyond an edge */
    if( lX < 0.0 ) {
      diff = -lX;
      lX = 0.0;
      rX += diff;
      coord->ib.cenX += diff;
    } else if( rX > (double)coord->img.width ) {
      diff = rX - (double)coord->img.width;
      lX -= diff;
      rX = (double)coord->img.width;
      coord->ib.cenX -= diff;
    }
    if( tY < 0 ) {
      diff = -tY;
      tY = 0;
      bY += diff;
      coord->ib.cenY += diff;
    } else if( bY > (double)coord->img.height ) {
      diff = bY - (double)coord->img.height;
      tY -= diff;
      bY = (double)coord->img.height;
      coord->ib.cenY -= diff;
    }
    /* set edges to integer pixel coords, rounding in from edge */
    coord->ib.srcX1 = (int)(lX + 0.001);
    coord->ib.srcY1 = (int)(tY + 0.001);
    coord->ib.srcX2 = (int)(rX - 0.001);
    coord->ib.srcY2 = (int)(bY - 0.001);
    /* compute transforms */
    set_transform(coord->ib.cenX, coord->ib.cenY, coord->ib.zoom,
		  coord->ib.zoom, &coord->buf, &coord->img,
		  &coord->buftoimg, &coord->imgtobuf);
    combine_transform(&coord->buftofile, &coord->buftoimg, &coord->imgtofile);
    set_edges(&coord->buftofile, &coord->file, &coord->buf, &coord->fb);
    /* compute inverse of buftofile (used to stuff pixels from socket) */
    invert_transform(&coord->filetobuf, &coord->buftofile, coord->file.ioff); 
  }
}

/*
 * Subroutine:	set_edges
 * Purpose:	Set up parameter to map image data from src (A) to dst (B)
 *		Set up the edges of dst sys (B) as plotted in src (A)
 * Note:	Used to set up transfer of image data from A to B
 */
void set_edges ( BAtrans, Asys, Bsys, ABmap )
     Transform *BAtrans;
     Coordsys *Asys, *Bsys;
     Edges *ABmap;
{
  float lX, tY, rX, bY, temp;
  float BAzoom;
  void d_transform();

  /* compute four corner pixel coords */
  d_transform(BAtrans, Bsys->X1, Bsys->Y1, &lX, &tY);
  d_transform(BAtrans, Bsys->X2, Bsys->Y2, &rX, &bY);
  /* account for any rotation or inversion */
  if( lX > rX ) {
    temp = lX;
    lX = rX;
    rX = temp;
  }
  if( tY > bY ) {
    temp = tY;
    tY = bY;
    bY = temp;
  }
  /* clip AB area by src (A) area */
  ABmap->clip = 0;
  if( lX < Asys->X1 ) {
    lX = Asys->X1;
    ABmap->clip += LX;
  }
  if( rX > Asys->X2 ) {
    rX = Asys->X2;
    ABmap->clip += RX;
  }
  if( tY < Asys->Y1 ) {
    tY = Asys->Y1;
    ABmap->clip += TY;
  }
  if( bY > Asys->Y2 ) {
    bY = Asys->Y2;
    ABmap->clip += BY;
  }
  /* round any edges inward */
  ABmap->srcX1 = (int)(lX + 0.001);
  ABmap->srcY1 = (int)(tY + 0.001);
  ABmap->srcX2 = (int)(rX - 0.001);
  ABmap->srcY2 = (int)(bY - 0.001);
  ABmap->srcXwdth = (int)(0.0001 + rX - lX);
  ABmap->srcYhght = (int)(0.0001 + bY - tY);
  /* get AB block and integer block code (opposite of BA) */
  /* assume symetric orthogonal blocking - one factor is 0, other is block */
  if( BAtrans->inx_outx > 0.000001 )
    BAzoom = BAtrans->inx_outx;
  else if( BAtrans->inx_outx < -0.000001 )
    BAzoom = -BAtrans->inx_outx;
  else if( BAtrans->iny_outx > 0.000001 )
    BAzoom = BAtrans->iny_outx;
  else
    BAzoom = -BAtrans->iny_outx;
  /* block code is integer factor (-1/block for fractional block) */
  /* block is inverse of zoom in our usage of zoom */
  ABmap->zoom = 1.0 / BAzoom;
  if( BAzoom > 0.75 )
    ABmap->block = (int)(BAzoom + 0.5);
  else
    ABmap->block = -(int)(ABmap->zoom + 0.5);
  /* transfer basic mapping destination parameters */
  ABmap->dstX1 = Bsys->X1i;
  ABmap->dstY1 = Bsys->Y1i;
  ABmap->dstX2 = Bsys->X2i;
  ABmap->dstY2 = Bsys->Y2i;
  ABmap->dstXwdth = Bsys->Xwdth;
  ABmap->dstYhght = Bsys->Yhght;
}
