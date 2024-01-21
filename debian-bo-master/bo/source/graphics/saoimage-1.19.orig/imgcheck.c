#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	imgcheck.c (Image Check)
 * Purpose:	Check parameters for consistency and check image file size
 * Subroutine:	check_image()			returns: int
 * Xlib calls:	none
 * Unix calls:	stat()
 * Copyright:	1994 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Eric Mandel, M VanHilst initial version    9 January 1989
 *		{1} Doug Mink  .fit and .fts are FITS, too    28 October 1994
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>			/* stderr, NULL, etc. */
#include <math.h>			/* define sqrt */
#include <sys/types.h>
#include <sys/stat.h>			/* define stat */
#include "hfiles/constant.h"		/* define codes */
#include "hfiles/image.h"
#include "hfiles/cmdparse.h"		/* define parse status bits */

/*
 * Subroutine:	check_image
 * Purpose:	Do some consistency checks on image type and size
 * Returns:	0 if no errors found, else -1
 */
int check_image ( img, got_status )
     struct imageRec *img;
     int got_status;
{
  int len;
  static int check_array();

  /* if no image type given, check name suffix or assume an array */
  if( (got_status & CMD_FTYPE) == 0 ) {
    if( img->filename != NULL ) {
      len = strlen(img->filename);
      if( strcmp(&img->filename[len - 5], ".fits") == 0 ) {
	img->file_type = SOP_FITS;
      } else if( strcmp(&img->filename[len - 5], ".FITS") == 0 ) {
	img->file_type = SOP_FITS;
      } else if( strcmp(&img->filename[len - 4], ".fit") == 0 ) {
	img->file_type = SOP_FITS;
      } else if( strcmp(&img->filename[len - 4], ".FIT") == 0 ) {
	img->file_type = SOP_FITS;
      } else if( strcmp(&img->filename[len - 4], ".fts") == 0 ) {
	img->file_type = SOP_FITS;
      } else if( strcmp(&img->filename[len - 4], ".FTS") == 0 ) {
	img->file_type = SOP_FITS;
#ifdef OIF
      } else if( strcmp(&img->filename[len - 4], ".imh") == 0 ) {
	img->file_type = SOP_IRAF;
#endif
      } else
	img->file_type = SOP_Array;
    } else
      /* if no name, default */
      img->file_type = SOP_Logo;
  }
  /* check for file existence and validity early on */
  if( (img->file_type != SOP_Imtool) &&
      (img->file_type != SOP_PROS) &&
      (img->file_type != SOP_Logo) &&
      (img->filename != NULL) ) {
    struct stat statbuf;
    if( stat(img->filename, &statbuf) <0 ) {
      (void)fprintf(stderr,
		    "Error: cannot access image: %s\n", img->filename);
      perror(img->filename);
      return( -1 );
    }
  }
  /* for arrays, we can make some checks and some calculations */
  if( img->file_type == SOP_Array ) {
    if( check_array(img) <0 ) {
      return( -1 );
    }
  }
  /* conflicting zoom factors? */
  if( img->fdblock == 0 ) {
    img->fdblock = img->fiblock;
  } else if( (img->fdblock < img->fiblock) &&
	     ((img->fdblock % img->fiblock) != 0) ) {
    (void)fprintf(stderr, "Error: incompatible display and buffer zooms\n");
    return( -1 );
  }
  /* if we are dealing with fits files on a little endian (vax), swap bytes */
#ifdef LSB
  /* Standard FITS images are fixed order (not in native order to VAX) */
  if( img->file_type == SOP_FITS )
    img->byte_swap = !img->byte_swap;
#endif
  return( 0 );
}

/*
 * Subroutine:	check_array
 * Purpose:	Check array size vs file size etc
 * Returns:	0 if size determined and/or OK, else -1
 */
static int check_array ( img )
     struct imageRec *img;
{
  long fsize;
  long arrsize;
  float posdim;		/* possible dimension */
  int headersize;	/* size in bytes of header */
  int rawsize;
  static long size_imagefile();

  /* get header size */
  headersize = img->headersize;
  /* get file size */
  fsize = size_imagefile(img, headersize, &rawsize);
  /* we might be able to figure out the dimensions for a square */
  if( (img->filerows == 0) || (img->filecols == 0) ) {
    /* if there is an image data type specified, try it */
    if( img->storage_type != ARR_None ){
      posdim = (int)sqrt((double)fsize);
      if( fsize == (posdim * posdim) ){
	img->filecols = (int)posdim;
	img->filerows = (int)posdim;
      } else {
	(void)fprintf(stderr, "File size neither given nor square: %d (%d)\n",
		      rawsize, fsize);
	return( -1 );
      }
    } else {
      /* check 2, 4 byte data size */
      /* check for I2 default */
      fsize /= 2;
      posdim = (int)sqrt((double)fsize);
      if( fsize == (posdim * posdim) ){
	img->storage_type = ARR_I2;
      } else {
	/* check for R4 default */
	fsize /=  2;
	posdim = (int)sqrt((double)fsize);
	if( fsize == (posdim * posdim) ){
	  img->storage_type = ARR_R4;
	} else {
	  (void)fprintf(stderr, "File size not square I2 or R4: %d (%d)\n",
			rawsize, rawsize - headersize);
	  return( -1 );
	}
      }
      img->filecols = posdim;
      img->filerows = posdim;
    }
  } else if( img->storage_type == ARR_None ) {
    /* get product of array dimensions in bytes */
    arrsize = img->filecols * img->filerows;
    /* we might find pixel size for a default, if only rows&cols given */
    if( fsize == arrsize ) {
      img->storage_type = ARR_U1;
    } else if( fsize == (arrsize*2) ) {
      img->storage_type = ARR_I2;
    } else if( fsize == (arrsize*4) ) {
      img->storage_type = ARR_R4;
    } else if( fsize == (arrsize*8) ) {
      img->storage_type = ARR_R8;
    } else {
      (void)fprintf(stderr, "File size does not scale to any type: %d (%d)\n",
		    rawsize, rawsize - headersize);
      return( -1 );
    }
  } else {
    /* in any case, image must not exceed file */
    arrsize = img->filecols * img->filerows;
    if( arrsize > fsize ) {
      (void)fprintf(stderr, "Request (%d) exceeds file size: %d (%d)\n",
		    arrsize, rawsize, fsize);
      return( -1 );
    }
  }
  return( 0 );
}

/*
 * Subroutine:	size_imagefile
 * Purpose:	return size of file adjusted for header and storage type
 */
static long size_imagefile ( img, headersize, rawsize )
     struct imageRec *img;
     int headersize;		/* given size of file header area */
     int *rawsize;		/* raw file size in bytes */
{
  struct stat buf;
  int fsize;

  /* get the file length in bytes */
  (void)stat(img->filename, &buf);
  *rawsize = buf.st_size;
  /* computed the equivalent array size */
  switch( img->storage_type ) {
  case ARR_R8:
    fsize = (*rawsize - headersize) / sizeof(double);
    break;
  case ARR_I4:
  case ARR_R4:
    fsize = (*rawsize - headersize) / sizeof(float);
    break;
  case ARR_U2:
  case ARR_I2:
    fsize = (*rawsize - headersize) / sizeof(short);
    break;
  case ARR_U1:
  default:
    fsize = *rawsize - headersize;
    break;
  }
  return( fsize );
}
