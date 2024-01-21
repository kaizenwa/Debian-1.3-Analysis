#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	readiraf.c (Read IRAF)
 * Purpose:	Read "old style" IRAF OIF (.imh) files directly
 * Copyright:	1989,1995,1996 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version	     23 January 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *              {2} Doug Mink IRAF spatial WCS implemented        12 Dec 1995
 *              {3} Doug Mink No message if header too small      14 Dec 1995
 *              {4} Doug Mink IRAF2FITS arguments revised          3 Jan 1996
 *              {5} Doug Mink Allocate actual header size          9 Feb 1996
 *              {5} Doug Mink Change IRAF2FITS arguments           9 Feb 1996
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, FD, and NULL */
#include <unistd.h>		/* define lseek arguments */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#define strchr index
#define strrchr rindex
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>           /* X window stuff */
#include <X11/Xutil.h>          /* X window manager stuff */
#include "hfiles/constant.h"
#include "hfiles/struct.h"      /* declare structure types */
#include "hfiles/extern.h"      /* extern main parameter structure */


/* just read in a nice power of two enough to cover the interesting parts */
/* actual size is (LEN_IMHDR + 1) * sizeof(int) */
#define READHEAD 1024
/* parameters from iraf/lib/imhdr.h */
#define SZ_IMPIXFILE 79
#define LEN_IMHDR 513
/* offsets into header in sizeof(int) units for various parameters */
#define	IM_PIXTYPE	4		/* datatype of the pixels */
#define	IM_NDIM		5		/* number of dimensions */
#define	IM_PHYSLEN	13		/* physical length (as stored) */
#define	IM_PIXOFF	22		/* offset of the pixels */
#define	IM_CTRAN	52		/* coordinate transformations */
#define	IM_PIXFILE	103		/* name of pixel storage file */

/* The Coordinate Transformation Structure (IM_CTRAN) from iraf/lib/imhdr.h */
/* None of this stuff is supported by IRAF applications, yet */
#define	LEN_CTSTRUCT	50
#define	CT_VALID	0		/* (y/n) is structure valid? */
#define	CT_BSCALE	1		/* pixval scale factor */
#define	CT_BZERO	2		/* pixval offset */
#define	CT_CRVAL	3		/* value at pixel (for each dim) */
#define	CT_CRPIX	10		/* index of pixel (for each dim) */
#define	CT_CDELT	17		/* increment along axis for each dim */
#define	CT_CROTA	24		/* rotation angle (for each dim) */
#define	CT_CTYPE	36		/* coord units string */

/* codes from iraf/unix/hlib/iraf.h */
#define	TY_CHAR		2
#define	TY_SHORT	3
#define	TY_INT		4
#define	TY_LONG		5
#define	TY_REAL		6
#define	TY_DOUBLE	7
#define	TY_COMPLEX	8

#define LEN_IRAFHDR	15000
#define LEN_FITSHDR	11520

/*
 * Subroutine:	init_irafimh
 * Purpose:	Open and read the iraf .imh file and set up the pixfile
 		for reading
 * Returns:	-1 if failure, else FD to image file ready for reading data
 * Notes:	The imhdr format is defined in iraf/lib/imhdr.h, some of
 *		which defines or mimiced, above.
 */
int init_irafimh ( img )
     struct imageRec *img;
{
  int fd;
  int *header, nbfhead, ibpx;
  char *fitsheader;
  static int check_immagic(), open_pixfile();
  int open_disk(), lseek_disk(), read_disk();
  char *calloc_errchk();
  void close_disk();
  struct WorldCoor *wcsinit();
  int nbhead;

  /* open the image header file */
  if( (fd = open_disk(img->filename, IOP_Read, 0)) <= 0 )
    return( -1 );

  /* Find size of image header file */
  nbhead = (int) lseek (fd, 0, SEEK_END);
  (void) lseek (fd, 0, SEEK_SET);

  /* allocate initial sized buffer */
  header = (int *)calloc_errchk(nbhead, 1, "IRAF header");

  /* read in first block */
  if (read_disk(fd, (char *)header, LEN_IRAFHDR, 0,
		img->filename, "IRAF header") < READHEAD ) {
    free((char *)header);
    close_disk(fd, img->filename);
    return( -1 );
  }
  close_disk (fd, img->filename);

  /* check header magic word */
  if (check_immagic((char *)header) ) {
    free ((char *)header);
    (void)fprintf(stderr, "File %s not valid SPP imhdr.\n", img->filename);
    return( -1 );
  }

  /* check number of image dimensions */
  if (header[IM_NDIM] < 2 ) {
    free ((char *)header);
    (void)fprintf(stderr, "File does not contain 2d image\n", img->filename);
    return(-1);
  }

  /* get file parameters */
  switch( header[IM_PIXTYPE] ) {
  case TY_SHORT:
    img->storage_type = ARR_I2;
    img->bytepix = 2;
    break;
  case TY_INT:
  case TY_LONG:
    img->storage_type = ARR_I4;
    img->bytepix = 4;
    break;
  case TY_REAL:
    img->storage_type = ARR_R4;
    img->bytepix = 4;
    break;
  case TY_DOUBLE:
    img->storage_type = ARR_R8;
    img->bytepix = 8;
    break;
  default:
    (void)fprintf(stderr,"Unsupported data type: %d\n",header[IM_PIXTYPE]);
    free((char *)header);
    return( -1 );
  }
  img->filecols = header[IM_PHYSLEN];
  img->filerows = header[IM_PHYSLEN+1];

  /* Check for existence and open pix file */
  if( (fd = open_pixfile((char *)&header[IM_PIXFILE],img->filename)) < 0 ) {
    free((char *)header);
    return( -1 );
  } 

  /* Check for data in pixel file */
  if( lseek_disk(fd, (header[IM_PIXOFF]-1) * sizeof(short), img->filename)
     < 0 ) {
    free((char *)header);
    close_disk(fd, img->filename);
    return( -1 );
  }

  /* Convert IRAF image header to FITS header */
  fitsheader = calloc_errchk (LEN_FITSHDR, sizeof(char), "FITS header");

/*  Set up pixel size in fits header */
  ibpx = img->bytepix * 8;
  nbfhead = iraf2fits (header, nbhead, fitsheader, LEN_FITSHDR);

  /* Initialize World Coordinate System */
  if (nbfhead > 0) {
    wcs = wcsinit (fitsheader);
    if (control.verbose)
      wcscent (wcs);
    wcscominit (wcs, &wcscommand);
    wcsoutinit (wcs, &wcscoor);
    }

  free ((char *)header);
  free (fitsheader);
  return (fd);
}

/*
 * Subroutine:	open_pixfile
 * Purpose:	Open the pixfile
 * Returns:	-1 if failure, else fd of open file
 * Note:	IRAF pixfile name includes machine name in case file system
 *		is specific to a machine (i.e. /tmp).  We try with machine
 *		name only if it cannot be read without the machine name.
 */
static int open_pixfile ( pixname, hdrname )
     char *pixname;
     char *hdrname;
{
  int fd;
  char *bang;
  int open_disk();
  static void pack_SPPstring(), same_path();

  pack_SPPstring(pixname, SZ_IMPIXFILE);
  if( strncmp(pixname, "HDR$", 4) == 0 )
    same_path(pixname, hdrname);
  if( (bang = strchr(pixname, '!')) != NULL ) {
    if( (fd = open_disk(bang + 1, IOP_Read, 0)) >= 0 )
      return( fd );
  }
  if( (fd = open_disk(pixname, IOP_Read, 0)) >= 0 ) {
    return( fd );
  }
  return( -1 );
}
    
/*
 * Subroutine:	check_immagic
 * Purpose:	verify that file is valid IRAF imhdr by checking first 5 chars
 * Returns:	0 on success, 1 on failure
 */
static int check_immagic ( line )
     char *line;
{
  static void pack_SPPstring();

  pack_SPPstring (line, 5);
  if( strncmp(line, "imhdr", 5) != 0 )
    return(1);
  else
    return(0);
}

/*
 * Subroutine:	pack_SPPstring
 * Purpose:	Convert ASCII string from SPP char per short to C char per byte
 */
static void pack_SPPstring ( line, len )
     char *line;
     int len;
{
  int i, j;

  /* adaptive byte selection (whichever byte order) chars alternate w/ \0 */
  if( line[0] == '\0' )
    j=1;
  else
    j=0;
  for( i=0; i<len; i++, j+=2 )
    line[i] = line[j];
  line[i] = '\0';
}

/*
 * Subroutine:	same_path
 * Purpose:	Put filename and header path together
 */
static void same_path ( pixname, hdrname )
     char *pixname, *hdrname;
{
  int len;
  char temp[SZ_IMPIXFILE];

  if( strncmp(pixname, "HDR$", 4) == 0 ) {
    /* load entire header name string into name buffer */
    (void)strncpy(temp, &pixname[4], SZ_IMPIXFILE);
    (void)strncpy(pixname, hdrname, SZ_IMPIXFILE);
    /* find the end of the pathname */
    len = strlen(pixname);
#ifndef VMS
    while( (len > 0) && (pixname[len-1] != '/') )
#else
    while( (len > 0) && (pixname[len-1] != ']') && (pixname[len-1] != ':') )
#endif
      len--;
    /* add name */
    pixname[len] = '\0';
    (void)strncat(pixname, temp, SZ_IMPIXFILE);
  }
}
