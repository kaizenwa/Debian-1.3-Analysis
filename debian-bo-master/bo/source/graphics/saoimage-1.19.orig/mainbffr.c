#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mainbffr.c (Main Buffers)
 * Purpose:	Allocate or reallocate large buffers
 * Subroutine:	init_imaging buffers()		returns: void
 * Subroutine:	init_display_buffers()		returns: void
 * Subroutine:	init_colorbarbuf()		returns: void
 * Subroutine:	init_panbuf()			returns: void
 * Subroutine:	init_dispbuf()			returns: void
 * Subroutine:	init_imagebuf()			returns: int
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  25 May 1989
 *              {1} MVH fix in init_imagebuf to note square buf  4 April 1990
 *              {2} Doug Mink add initial zoom factor              2 Jan 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* get stderr, NULL, etc */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, MAX, etc. */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/scale.h"	/* define scaling constants */

/*
 * Subroutine:	init_imaging_buffers
 * Purpose:	Allocate major buffers used for imaging
 */
void init_imaging_buffers ()
{
  char *calloc_errchk();
  int init_imagebuf();

  buffer.scalemap = (unsigned char *)
    calloc_errchk(SCALEBUFSZ, sizeof(char), "scaling map");
  buffer.histogram = (int *)
    calloc_errchk(SCALEBUFSZ, sizeof(int), "histogram table");
  (void)init_imagebuf ();
} 

/*
 * Subroutine:	init_display_buffers
 * Purpose:	Allocate buffers used for each window's display
 */
void init_display_buffers ()
{
  void init_panbuf(), init_dispbuf();

  init_panbuf();
  init_dispbuf();
} 

/*
 * Subroutine:	init_panbuf
 * Purpose:	Allocate or reallocate panbox display buffers
 */
void init_panbuf ()
{
  int buf_sz;
  char *calloc_errchk();
  
  buf_sz = panbox.xwidth * panbox.yheight;
  /* test if the current buffers are adequate, else allocate or reallocate */
  if( buf_sz > buffer.panbuf_sz ) {
    if( buffer.panbuf != NULL )
      free( (char *)buffer.panbuf );
    if( panbox.image.data != NULL ) {
      free( panbox.image.data );
      panbox.image.data = NULL;
      panbox.data_size = 0;
    }
    /* reserve exact window sized buffer for the panbox short integer image */
    buffer.panbuf = (short *)
      calloc_errchk(buf_sz, sizeof(short), "Panbox map");
    buffer.panbuf_sz = buf_sz;
    /* reserve buffer for the panbox display image */
    if( color.screen_depth <= 1 ) {
      /* if only bitmap will be used */
      panbox.image.bytes_per_line = (panbox.xwidth + 7) / 8;
      buf_sz = panbox.image.bytes_per_line * panbox.yheight;
    }
    panbox.image.data = calloc_errchk(buf_sz, sizeof(char), "Panbox display");
    panbox.data_size = buf_sz;
  }
  panbox.image.width = panbox.xwidth;
  panbox.image.height = panbox.yheight;
}

/*
 * Subroutine:	init_dispbuf
 * Purpose:	Allocate or reallocate dispbox display buffer
 */
void init_dispbuf ()
{
  int buf_sz;
  char *calloc_errchk();
  void set_coordsys(), set_tdisp();

  dispbox.image.width = dispbox.xwidth;
  dispbox.image.height = dispbox.yheight;
  if( color.screen_depth <= 1 ) {
    /* if only bitmap will be used */
    dispbox.image.bytes_per_line = (dispbox.xwidth + 7) / 8;
    buf_sz = dispbox.image.bytes_per_line * dispbox.yheight;
  } else
    buf_sz = dispbox.xwidth * dispbox.yheight;
  /* test if the current buffer is adequate, else allocate or reallocate */
  if( buf_sz > dispbox.data_size ) {
    if( dispbox.image.data != NULL )
      free( dispbox.image.data );
    dispbox.image.data = calloc_errchk(buf_sz, sizeof(char), "Display image");
    dispbox.data_size = buf_sz;
  }
  /* set the display coord system and propose new display parameters */
  set_coordsys(&coord.disp, 0.5, (int)dispbox.width, (int)dispbox.height,
	       dispbox.xzero, dispbox.yzero,
	       dispbox.xzero + dispbox.xwidth - 1,
	       dispbox.yzero + dispbox.yheight - 1);
  /* complete tdi parameters, must already have center and zoom */
  if (img.zoom > 0)
    coord.tid.zoom = img.zoom;
  set_tdisp(&coord);
}

/*
 * Subroutine:	init_imagebuf
 * Purpose:	Allocate or reallocate main image buffer to fit img or as
 *		much as allowed.  Also allocate file data buffer if data
 *		cannot be read directly into i*2 buf.
 * Returns:	1 on success, else 0.
 * Post-state:	buffer.load_filebuf set to 0 if alloc's failed.
 * Method:	Try to allocate enough for entire given image subsection.
 *		If the display is to be rotated, make the short buf square
 *		or double (whichever is less).
 * Parameters:
 *	buffer:		i/o: record structure for imaging buffers
 *	coord.img:	i: coordinate system of image or image subsection
 *	coord.buf:	o: coordinate system of short integer image buffer
 */
static char imgbuf_error[] =
  "Unable to allocate sufficient memory for requested image.\n";
int init_imagebuf ()
{
  int width, height;
  int img_sz, buf_sz;
  int shortbuf_double = 0;
  int shortbuf_square = 0;
  int did_alloc = 0;
  char *calloc_errchk();
  void set_coordsys();

  /* calculate buffer size, use img if within limit, else use limit */
  width = coord.img.width;
  height = coord.img.height;
  img_sz = width * height;
  /* if image is to be rotated 90% (aspect ratio changed) */
  if( img.rotate_code & 1 ) {
    /* make short buffer square or double for rotation algorithm */
    buf_sz = SQR(MAX(width, height));
    if( buf_sz > (2 * img_sz) ) {
      buf_sz = 2 * img_sz;
      shortbuf_double = 1;
    } else
      shortbuf_square = 1;
  } else
    buf_sz = img_sz;
  /* buf_sz is in shorts, img_sz will now be in bytes (as is filebuf_sz) */
  img_sz *= img.bytepix;
  /* free filebuf if unique and inadequate, or unneeded */
  if( buffer.filebuf == (char *)buffer.shortbuf ) {
    buffer.filebuf = NULL;
    buffer.filebuf_sz = 0;
  } else if( (buffer.filebuf != NULL) &&
	     ((img.bytepix <= 2) || (img_sz > buffer.filebuf_sz)) ) {
    free( buffer.filebuf );
    buffer.filebuf = NULL;
    buffer.filebuf_sz = 0;
  }
  /* check the adequacy of current short integer image buf */
  if( buf_sz > buffer.shortbuf_sz ) {
    if( buffer.shortbuf != NULL )
      free( (char *)buffer.shortbuf );
    buffer.shortbuf = (short *)calloc_errchk(buf_sz, sizeof(short), NULL);
    if( buffer.shortbuf == 0 ) {
      /* if failed, clear the slate, report error, and continue */
      buffer.shortbuf_sz = 0;
      if( buffer.filebuf != NULL ) {
	free( buffer.filebuf );
	buffer.filebuf = NULL;
	buffer.filebuf_sz = 0;
      }
      (void)fprintf(stderr, imgbuf_error);
      buffer.load_filebuf = 0;
      return( 0 );
    }
    buffer.shortbuf_sz = buf_sz;
    did_alloc = 1;
  }
  /* check on file data buf (if it still exists, it must be adequate) */
  if( buffer.filebuf == NULL ) {
    if( img.bytepix > 2 ) {
      /* force worst case allignment */
      buffer.filebuf = calloc_errchk((img_sz + 7) / 8, 8, NULL);
      if( buffer.filebuf == NULL ) {
	/* if failed, clear the slate, report error, and continue */
	buffer.filebuf_sz = 0;
	free( (char *)buffer.shortbuf );
	buffer.shortbuf = NULL;
	buffer.shortbuf_sz = 0;
	fputs (imgbuf_error, stderr);
	buffer.load_filebuf = 0;
	return( 0 );
      }
      buffer.filebuf_sz = img_sz;
      did_alloc++;
    } else {
      /* no special input buffer needed */
      buffer.filebuf = (char *)buffer.shortbuf;
      /* shortbuf_sz is shorts, filebuf_sz is bytes */
      buffer.filebuf_sz = buffer.shortbuf_sz * sizeof(short);
    }
  }
  /* note rotation space status */
  buffer.shortbuf_double = shortbuf_double;
  buffer.shortbuf_square = shortbuf_square;
  /* set basic buffer coord system parameters (fill buffer) */
  set_coordsys(&coord.buf, 0.5, width, height, 0, 0, width - 1, height - 1);
  /* flag that buffer must be loaded */
  coord.buferror = 1;
  buffer.load_filebuf = 1;
  /* report buffer size */
  if( control.verbose && did_alloc ) {
    (void)printf("Reserved %d x %d image data buffer", width, height);
    if( did_alloc > 1 )
      (void)printf("s.\n");
    else
      (void)printf(".\n");
  }
  return( 1 );
}
