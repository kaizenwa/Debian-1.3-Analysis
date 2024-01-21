#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readfits.c (Read FITS)
 * Purpose:	Read a FITS file header and prepare file for basic array read
 * Subroutine:	int init_fits()
 * Note:	Only SIMPLE = T is permitted, but will handle BITPIX = 8, 16,
 *		32, -16(u_short), -32(float), -64(double).  If img->nimage
 *		is set to a value greater than 1, file will be lseeked to the
 *		nimage'th image (assuming there are several images stored as
 *		a stack (on 3rd dimension).
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     23 January 1989
 *              {1} Doug Mink           implement nimage     11 October 1990
 *              {2} Doug Mink           fix dimensions        3 May     1995
 *              {3} Doug Mink           move memset          18 October 1995
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, NULL, etc. */
#include <X11/Xlib.h>		/* needed for control.h */
#include "hfiles/constant.h"
#include "hfiles/control.h"	/* define IOP codes */
#include "hfiles/image.h"

#define FITSBLOCK 2880

/*
 * Subroutine:	init_fits
 * Purpose:	Open fits file, get parameters and get ready to read data.
 * Returns:	-1 if failure, else fd of file, positioned at start of data
 * Note:	Data read will be handled as standard array read.
 */
int init_fits ( img )
     struct imageRec *img;
{
  float scale, bias;
  int fd;
  int headlen, image_start;
  int bitpix, naxis, naxes[3];
  char *header;
  static char *load_fitsheader();
  int read_fitsheader(), open_disk(), lseek_disk();
  void close_disk();

  /* open the image file */
  if( (fd = open_disk(img->filename, IOP_Read, 0)) <= 0 )
    return( -1 );
  headlen = img->headersize;
  /* read and interpret the header */
  if( (header = load_fitsheader(fd, &headlen, img->filename)) == NULL ) {
    close_disk(fd, img->filename);
    return( -1 );
  }
  if( read_fitsheader(header, headlen, &bitpix, &naxis, naxes, &scale, &bias)
      != 0 ) {
    free(header);
    switch( bitpix ) {
    case 8:
      img->storage_type = ARR_U1;
      img->bytepix = 1;
      break;
    case 16:
      img->storage_type = ARR_I2;
      img->bytepix = 2;
      break;
    case 32:
      img->storage_type = ARR_I4;
      img->bytepix = 4;
      break;
    case -16:
      img->storage_type = ARR_U2;
      img->bytepix = 2;
      break;
    case -32:
      img->storage_type = ARR_R4;
      img->bytepix = 4;
      break;
    case -64:
      img->storage_type = ARR_R8;
      img->bytepix = 8;
      break;
    default:
      (void)fprintf(stderr,"Illegal FITS BITPIX: %d\n",bitpix);
      close_disk(fd, img->filename);
      return( -1 );
    }
    if( img->nimage > 1 ) {
      if( (naxis <= 2) || (naxes[2] < img->nimage) ) {
	(void)fprintf(stderr,
		      "Only %d images in file %s\n", naxis, img->filename);
	if (naxes[2] > 0)
	    img->nimage = naxes[2];
	else
	    img->nimage = 1;

	/* close_disk(fd, img->filename);
	return( -1 ); */
      }
      image_start = headlen + 
			((img->nimage-1) * naxes[0] * naxes[1] * img->bytepix);
      if( lseek_disk(fd, image_start, img->filename) < 0 ) {
	img->headersize = 0;
	close_disk(fd, img->filename);
	return( -1 );
      }
    } else {
      img->headersize = headlen;
    }
    img->filecols = naxes[0];
    img->filerows = naxes[1];
    if( (scale != 1.0) || (bias != 0.0) ) {
      img->fscale = scale;
      img->fbias = bias;
      img->fscaled = 1;
    } else
      img->fscaled = 0;
    return( fd );
  } else {
    free(header);
    close_disk(fd, img->filename);
    return( -1 );
  }
}


/*
 * Subroutine:	load_fitsheader
 * Purpose:	Load the FITS header from the disk file into memory.
 * Notes:	FITS headers are made in block units (2880 bytes each).
 *		If length is not initially zero, it is taken as a directive
 *		to override FITS standard (header of given size is loaded).
 *		FITS header must have "SIMPLE =" in first 10 bytes (and 'T'
 *		in next 20 for ximage use).
 */
static char *load_fitsheader ( fd, length, filename )
     int fd;
     int *length;
     char *filename;
{
  int headlen, nbytes;
  char *header;
  char TorF[4];
  static char *get_keyfield();
  char *realloc(), *calloc_errchk();
  int read_disk();
  void no_fitscomment();

  if( *length > 0 )
    headlen = *length;
  else
    headlen = FITSBLOCK;
  /* allocate initial sized buffer */
  header = calloc_errchk (headlen+1, 1, "FITS header");
  /* read in first block */
  if( (nbytes = read_disk(fd, header, headlen, 1, filename, "FITS header"))
     != headlen ) {
    free (header);
    return( NULL );
  }
  /* consistency check for FITS header */
  if( strncmp(header, "SIMPLE  =", 9) != 0) {
    (void)fprintf(stderr,"No SIMPLE keyword in FITS header");
    free(header);
    return( NULL );
  }
  /* we only support SIMPLE = T */
  no_fitscomment(header+10, 20);
  if( (sscanf(header+10, "%1s", TorF) <= 0) || (TorF[0] != 'T') ) {
    (void)fprintf(stderr,"SIMPLE = %c not supported\n", TorF[0]);
    free(header);
    return( NULL );
  }
  if( *length != 0 ) {
    /* if header length was given, it must check out with an END */
    if( get_keyfield(header, "END     ", headlen, 0) == NULL ) {
      (void)fprintf(stderr,"%d byte FITS header has no END card!\n", headlen);
      free(header);
      return( NULL );
    }
  } else {
    /* if END keyword not found, read in more header until END is found */
    while( get_keyfield(header, "END     ", headlen, 0) == NULL ) {
      if ((header = realloc(header,(unsigned)(headlen+FITSBLOCK+1))) == NULL) {
	fputs("Reallocation failure reading header\n", stderr);

	return( NULL );
      }
      /* initialize the newly allocated memory */
      memset (header + headlen, 0, FITSBLOCK + 1);
      if( (nbytes = read_disk(fd, header+headlen, FITSBLOCK, 1,
			      filename, "extended header")) != FITSBLOCK ) {
	(void)fprintf(stderr, "Read %d bytes of header with no END card!\n",
		      headlen + nbytes);
	(void)fflush(stderr);
	free( header );
	return( NULL );
      }
      headlen += FITSBLOCK;
    }
    *length = headlen;
  }
  return( header );
}


/*
 * Subroutine:	get_keyfield
 * Purpose:	Return the data field for a given FITS header keyword
 * Returns:	If key not found, return NULL (0).
 */
static char *get_keyfield ( header, keyword, length, report_error )
     char *header;	/* buffer start */
     char *keyword;	/* keyword to match */
     int length;	/* if zero, search up to "END" keyword */
     int report_error;	/* if > 0, fatal if key not found */
{
  int key_not_end, i;
  void no_fitscomment();

  key_not_end = (strncmp(keyword, "END     ", 8) != 0);
  for( i=0; i<length; i+=80 ) {
    /* check for END keyword marking end of header, unless END is the key */
    if( key_not_end && strncmp(header+i,"END     ",8) == 0 )
      break;
    /* check for desired keyword */
    if( strncmp(header+i,keyword,8) == 0 ) {
      no_fitscomment (header+i+10, 20);
      return( header+i+10 );
    }
  }
  if( report_error )
    (void)fprintf(stderr, "No `%s' keyword in FITS header\n", keyword);
  return( (char *)NULL );
}
