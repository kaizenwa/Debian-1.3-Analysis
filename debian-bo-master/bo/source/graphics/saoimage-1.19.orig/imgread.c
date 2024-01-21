#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgread.c (Image Read)
 * Purpose:	Read image headers and image files
 * Subroutine:	init_image()			returns: int
 * Subroutine:	load_image()
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	         1 July 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <X11/Xlib.h>			/* X window stuff */
#include <X11/Xutil.h>			/* X window manager stuff */
#include "hfiles/constant.h"		/* constants and codes */
#include "hfiles/struct.h"		/* declare structure types */
#include "hfiles/extern.h"		/* extern main parameter structures */

static int init_imtool;

/*
 * Subroutine:	init_image
 * Purpose:	Set the image dimensions
 * Returns:	1 if OK, 0 if trouble
 * Method:	set image info from known values or read from a header
 */
int init_image ( )
{
  int get_fbconfig();		/* get IRAF expected frame size */
  void init_img(), init_dispcen();
#ifdef FITS
  int init_fits();
#endif
#ifdef OIF
  int init_irafimh();
#endif

  /* read and strip subsection suffix */
/* %%  get_subsection(&img); */
  switch( img.file_type ) {
  case SOP_Array:
#ifdef DEBUG
    if( (img.filerows <= 0) || (img.filecols <= 0) ) {
      (void)fprintf(stderr, "Error: array dimensions not set");
      return( 0 );
    }
#endif
    img.block_type = SOP_ZoomNone;
    break;
  case SOP_FITS:
#ifdef FITS
    if( (img.fd = init_fits(&img)) < 0 )
      return( 0 );
    img.block_type = SOP_ZoomNone;
    break;
#else
    (void)fprintf(stderr, "WARNING: not compiled with -DFITS switch\n");
    return( 0 );
#endif
  case SOP_IRAF:
#ifdef OIF
    if( (img.fd = init_irafimh(&img)) < 0 )
      return( 0 );
    img.block_type = SOP_ZoomNone;
    break;
#else
    (void)fprintf(stderr, "WARNING: not compiled with -DIRAF switch\n");
    return( 0 );
#endif
  case SOP_SAOCCD:
#ifdef SAOCCD
    init_saoccd(&img);
    img.block_type = SOP_ZoomNone;
    break;
#else
    (void)fprintf(stderr, "WARNING: not compiled with -DSAOCCD switch\n");
    return( 0 );
#endif
  case SOP_Imtool:
  case SOP_PROS:
#ifdef IMTOOL
    /* remote access from iraf, start with standard screen size */
fprintf (stdout,"DBG: img.fbconfig=%d  -> ",img.fbconfig);
    if( get_fbconfig(img.fbconfig, &img.filecols, &img.filerows) == 0 ) {
      img.filecols = 512;
      img.filerows = 512;
    }
fprintf (stdout,"filecols=%d  filerows=%d\n",img.filecols,img.filecols);fflush(stdout);
    img.block_type = SOP_ZoomNone;
    /* signal load_image to load a default image */
    init_imtool = 1;
    break;
#else
    (void)fprintf(stderr, "Note: IRAF pipe support not compiled.\n");
    return( 0 );
#endif
  case SOP_Logo:
  default:
    img.filerows = 512;
    img.filecols = 512;
    img.block_type = SOP_ZoomNone;
    break;
  }
  /* fix up image record parameters and img to file conversions */
  init_img(&img, &coord);
  init_dispcen(&img, &coord);
  return( 1 );
}

/*
 * Subroutine:	load_image
 * Purpose:	Open if necessary, then read the appropriate image file
 *		and create a 16 bit image array
 */
void load_image ( fb, buf, bftrans, imgbuf, filebuf, buf_squared, buf_doubled )
     Edges *fb;			/* coord structures (for disp or pan) */
     Coordsys *buf;
     Transform *bftrans;
     short *imgbuf;
     char *filebuf;
     int buf_squared, buf_doubled;	/* buf is larger to enable rotation */
{
  static int init = 1;	/* signal first time through */
  int x, y;
  int width, height;
  int read_image;	/* avoid doing rotate if there is nothing to rotate */
  void read_array(), rotate_buf(), clear_coord_area(), load_logo();

#ifdef SUBSEC
  float X, Y;
  void i_transform();
  /* THIS ISN'T RIGHT (WHAT IF IMAGE IS ROTATED OR FLIPPED?) */
  /* get file coords of buffer's 0,0 element */
  i_transform(bftrans, 0, 0, &X, &Y);
  x = (int)(X + 0.5);
  y = (int)(Y + 0.5);
#else
  x = 0;
  y = 0;
  fb->block = 1;
#endif
  /* if rotation has a 90 degree component, get cross zoom factor */
  if( img.rotate_code & 1 ) {
    width = buf->height;
    height = buf->width;
  } else {
    width = buf->width;
    height = buf->height;
  }
  read_image = 0;
  switch(img.file_type) {
  case SOP_Array:
    read_array(-1, &img, imgbuf, filebuf, width, height, x, y,
	       fb->block, control.verbose);
    read_image = 1;
    break;
#ifdef FITS
  case SOP_FITS:
#endif
#ifdef OIF
  case SOP_IRAF:
#endif
    read_array(img.fd, &img, imgbuf, filebuf,
	       width, height, x, y, fb->block, control.verbose);
    read_image = 1;
    break;
#ifdef SAOCCD
  case SOP_SAOCCD:
    load_saoccd(&img, imgbuf, width, height, x, y, fb->block);
    read_image = 1;
    break;
#endif
#ifdef XRAY
  case SOP_HRI:
  case SOP_IPC:
    load_einstein(&img, imgbuf, width, height,
		  fb->cenX, fb->cenY, fb->block);
    read_image = 1;
    break;
#endif
#ifdef ROSAT
  case SOP_ROSAT:
    load_rosat(&img, imgbuf, width, height, fb->cenX, fb->cenY, fb->block);
    read_image = 1;
    break;
#endif
#ifdef IMTOOL
  case SOP_Imtool:
  case SOP_PROS:
    if( init )
      load_logo(imgbuf, width, height);
    if( init_imtool ) {
      init_imtool = 0;
    }
    break;
#endif
  case SOP_Logo:
    if( init )
      load_logo(imgbuf, width, height);
  default:
    break;
  }
  if( read_image ) {
    if( (img.row_order != 0) || (img.rotate_code != 0) ) {
      rotate_buf(imgbuf, img.row_order, img.rotate_code, width, height,
		 buf_squared, buf_doubled);
    }
    if( (img.file_type != SOP_Imtool) && (img.file_type != SOP_PROS) ) {
      /* we loaded a non-imtool image */
      img.imtool_200 = 0;
      /* if there was a second coordinate system, cancel it */
      if( coord.imtool_aux ) {
	clear_coord_area();
	coord.imtool_aux = 0;
      }
      /* if artificial limits were applied to input, mark short_buf extremes */
      if( img.fimin < img.fimax ) {
	buffer.clipmin = (int)((img.fimin - img.fibias) / img.fiscale);
	buffer.clipmax = (int)((img.fimax - img.fibias) / img.fiscale);
      } else {
	/* imtool limits are set when an image is read from the pipe */
	buffer.clipmin = -32768;
	buffer.clipmax = 32767;
      }
    }
  }
}
