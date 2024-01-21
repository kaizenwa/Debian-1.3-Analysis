#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgparam.c (Image Parameters)
 * Purpose:	Resolve missing image and display parameters
 * Subroutine:	init_img()			returns: void
 * Subroutine:	init_dispcen()			returns: void
 * Xlib calls:	none
 * Copyright:	1988 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	       31 August 1988
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include "hfiles/coord.h"		/* coord structs */
#include "hfiles/define.h"		/* MIN, MAX, and more */
#include "hfiles/image.h"		/* image struct */

/*
 * Subroutine:	init_img
 * Purpose:	Resolve readable subsection selection and fill in
 *		missing params
 * Called by:	init_image in ImageRead.c
 * Called by:	imtool_reinit in RemoteImtool.c
 */
void init_img ( image, coord )
     struct imageRec *image;
     struct coordRec *coord;
{
  double ioff;
  int width, height;
  int low_edge, wide_edge, high_edge;
  void set_coordsys(), set_imgtran();

  /* set img parameters */
  /* file to buffer must never use replication */
  if( image->fiblock < 1 ) image->fiblock = 1;
  /* file coordinates must work for either 0 based or 1 based indexing */
  /* offset of 0.5 is used in 0 based indexing, 0.0 is used in 1 based */
  low_edge = image->index_base;
  if( low_edge == 0 ) {
    ioff = 0.5;
    wide_edge = image->filecols - 1;
    high_edge = image->filerows - 1;
  } else {
    ioff = 0.0;
    wide_edge = image->filecols;
    high_edge = image->filerows;
  }
  /* clip inputs to file coordinate limits */
  if( image->fiX1 < low_edge ) image->fiX1 = low_edge;
  if( image->fiY1 < low_edge ) image->fiY1 = low_edge;
  if( image->fiX2 > wide_edge ) image->fiX2 = wide_edge;
  if( image->fiY2 > high_edge ) image->fiY2 = high_edge;
  width = image->fiX2 - image->fiX1 + 1;
  height = image->fiY2 - image->fiY1 + 1;
  /* if image section not set or incorrectly set, default to full file */
  if( (width <= 1) || (height <= 1) ) {
    width = image->filecols;
    height = image->filerows;
    image->fiX1 = low_edge;
    image->fiY1 = low_edge;
    image->fiX2 = wide_edge;
    image->fiY2 = high_edge;
  }
  /* set up the description of the file coordinate system */
  set_coordsys(&coord->file, ioff, image->filecols, image->filerows,
	       image->fiX1, image->fiY1, image->fiX2, image->fiY2);
  image->ficenX = coord->file.cenX;
  image->ficenY = coord->file.cenY;
  /* calculate size of img (check for 1 since it is the most common case) */
  if( image->fiblock != 1 ) {
    width /= image->fiblock;
    height /= image->fiblock;
  }
  /* account for rotation of the axes in going from file to display */
  /* set up the description of the img coordinate system */
  if( image->rotate_code & 1 ) {
    set_coordsys(&coord->img, 0.5, height, width,
		 0, 0, height - 1, width - 1);
  } else {
    set_coordsys(&coord->img, 0.5, width, height,
		 0, 0, width - 1, height - 1);
  }
  /* create the transformation matrices between img and file */
  set_imgtran(image, &coord->img, &coord->file,
	      &coord->imgtofile, &coord->filetoimg);
}

/*
 * Subroutine:	init_dispcen
 * Purpose:	Adjust chosen or default display parameters
 * Called by:	init_image in ImageRead.c
 * Called by:	imtool_reinit in RemoteImtool.c
 */
void init_dispcen ( image, coord )
     struct imageRec *image;
     struct coordRec *coord;
{
  int spec;
  void d_transform();

  /* if disp size is not given choose one within limits */
  if( image->dispcols <= 0 )
    image->dispcols = MAX( MIN( 512, coord->img.width ), 200 );
  if( image->disprows <= 0 )
    image->disprows = MAX( MIN( 512, coord->img.height ), 200 );
  /* set center of display, or use that which was given */
  spec = 0;
  if( image->fdcenX > 0 ) {
    if( image->fdcenX < image->fiX1 ) image->fdcenX = image->fiX1;
    if( image->fdcenX > image->fiX2 ) image->fdcenX = image->fiX2;
    spec = 1;
  } else
    image->fdcenX = image->ficenX;
  if( image->fdcenY > 0 ) {
    if( image->fdcenY < image->fiY1 ) image->fdcenY = image->fiY1;
    if( image->fdcenY > image->fiY2 ) image->fdcenY = image->fiY2;
    spec = 1;
  } else
    image->fdcenY = image->ficenY;
  /* get display center in img coords */
  if( spec ) {
    /* translate from file to img coords */
    d_transform(&coord->filetoimg, image->fdcenX, image->fdcenY,
		&(coord->tid.cenX), &(coord->tid.cenY));
  } else {
    /* use default (center of img) */
    coord->tid.cenX = coord->img.cenX;
    coord->tid.cenY = coord->img.cenY;
  }
  /* calculate disptoimg zoom factor */
  /* allow only positive blocking when reading file */
  if( image->fiblock < 0 )
    image->fiblock = -image->fiblock;
  /* if display blocking not set, make it that of img */
  if( image->fdblock == 0 ) {
    image->fdblock = image->fiblock;
    coord->tid.zoom = 1.0;
    coord->tid.block = 1;
  } else {
    double dzoom;
    if( image->fdblock > 0 )
      dzoom = 1.0 / (double)image->fdblock;
    else
      dzoom = (double)-(image->fdblock);
    if( image->fiblock != 1 )
      dzoom *= image->fiblock;
    /* set id block and zoom (make sure zoom is integerized) */
    if( dzoom > 0.75 ) {
      coord->tid.block = -(int)(dzoom + 0.5);
      coord->tid.zoom = (double)(-coord->tid.block);
    } else {
      coord->tid.block = (int)((1.0 / dzoom) + 0.5);
      coord->tid.zoom = 1.0 / (double)coord->tid.block;
    }
  }
}
