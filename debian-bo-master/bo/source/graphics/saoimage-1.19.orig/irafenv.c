#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafenv.c (IRAF Environment)
 * Purpose:	Do things needed to coorinate paths and coordinates with IRAF
 * Subroutine:	update_wcs()		returns: int
 * Subroutine:	get_fbconfig()		returns: int
 * Subroutine:	set_path_iraf()		returns: void
 * Unix calls:	fopen(), fclose(), getenv()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  9 July 1989
 *		{1} Jay Travisano (STScI)    VMS changes          10 Nov 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <ctype.h>
#include <stdio.h>		/* define FILE, stderr, fopen(), fclose() */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include "hfiles/coord.h"
#include "hfiles/define.h"	/* SZ_FNAME, etc. */
#include "hfiles/image.h"
#include "hfiles/imtool.h"

/*
 * Subroutine:	set_path_iraf
 * Purpose:	Construct the pathname of a user datafile by IRAF convention.
 */
void set_path_iraf ( filename )
     char *filename;		/* i: root filename */
{
  static char temproot[64];
  char *udir, *getenv();

  /* were we passed an absolute pathname as input? */
  if( *filename == '/' ) {
    return;
  }
  (void)strcpy(temproot, filename);
  /* get defined directory in order of preference */
  if( (udir = getenv("WCSDIR")) == NULL )
    if( (udir = getenv("wcsdir")) == NULL )
      if( (udir = getenv("HOME")) == NULL )
	/* at this point we should be checking ~user/iraf/uparm */
#ifndef VMS
	udir = "/tmp";
#else
	udir = "TMP:";
#endif
  if( udir[strlen(udir)-1] == '/' )
    sprintf(filename, "%s%s", udir, temproot);
  else
#if VMS
    /* don't need slash */
    sprintf(filename, "%s%s", udir, temproot);
#else
    sprintf(filename, "%s/%s", udir, temproot);
#endif
}

#ifdef IMTOOL
static char title[SZ_FNAME];
/* static char comment[SZ_FNAME]; */

/*
 * Subroutine:	update_wcs 
 * Purpose:	Load the screen WCS, if not yet validated, from the user
 *		wcs file, if any.  (wcs =  "WORLD COORDINATE SYSTEM")
 * UNIX calls:	fopen(), fclose()
 * Note:	File format (two lines):
 *		image title (imtool header label string)\n
 *		a b c d tx ty (WCS coordinate transformation matrix)
 */
int update_wcs ( image, coord, frame_number, wcsbuf )
     struct imageRec *image;
     struct coordRec *coord;
     int frame_number;
     char *wcsbuf;
{
  float xx, yx, xy, yy, xo, yo;	/* l: wcs matrix values */
  float low, high;		/* l: scaling limits */
  int w_type;			/* l: scaling code */
  int tokens;			/* l: number of tokens successfully parsed */
  FILE *fp;
  char label[1024];		/* l: file title */
  char wcs_file[SZ_FNAME];
  char root_name[64];
  int guess_true_file_coords();
  void set_path_iraf(), rotate_transform(), set_trans_speed(), show_filename();
  void invert_transform(), combine_transform();

  if( wcsbuf == NULL ) {
    /* get location of WCSFILE from environment */
    sprintf(root_name, WCSFILE, frame_number);
    (void)strcpy(wcs_file, root_name);
    set_path_iraf(wcs_file);
    fp = fopen(wcs_file, "r");
    /* if environment not correctly set, try Imtool's defaults, then IRAF's */
    if( fp == NULL ) {
      sprintf(wcs_file, "%s%s\0", "uparm/", root_name);
      fp = fopen(wcs_file, "r");
    }
#ifndef VMS
    if( fp == NULL ) {
      sprintf(wcs_file, "%s%s\0", "~/iraf/uparm/", root_name);
      fp = fopen(wcs_file, "r");
    }
    if( fp == NULL ) {
      sprintf(wcs_file, "%s%s\0", "/tmp/", root_name);
      fp = fopen(wcs_file, "r");
    }
#else
    if( fp == NULL ) {
      char *getenv();

      (void)strcpy(wcs_file, getenv("HOME"));
      (void)strcat(&wcs_file[strlen(wcs_file)-1], ".IRAF.UPARM]");
      (void)strcat(wcs_file, root_name);
      fp = fopen(wcs_file, "r");
    }
    if( fp == NULL ) {
      sprintf(wcs_file, "%s%s\0", "TMP:", root_name);
      fp = fopen(wcs_file, "r");
    }
#endif
    if( fp == NULL ) {
      (void)fprintf(stderr,
		    "WARNING: cannot find imtool coordinate matrix - %s\n",
		    wcs_file);
      return( 0 );
    }
    /* get iraf coordinate transform matrix */
    tokens = fscanf(fp, "%[^\n]\n%f%f%f%f%f%f%f%f%d", label,
		    &xx, &yx, &xy, &yy, &xo, &yo, &low, &high, &w_type);
    (void)fclose(fp);
  } else {
    tokens = sscanf(wcsbuf, "%[^\n]\n%f%f%f%f%f%f%f%f%d", label,
		    &xx, &yx, &xy, &yy, &xo, &yo, &low, &high, &w_type);
  }
  if( tokens < 7 ) {
    (void)fprintf(stderr, "WARNING: error reading imtool.wcs file\n");
    coord->imgtofile.inx_outx = 1.0;
    coord->imgtofile.iny_outx = 0.0;
    coord->imgtofile.inx_outy = 0.0;
    coord->imgtofile.iny_outy = 1.0;
    coord->imgtofile.iadd_outx = 0.5;
    coord->imgtofile.iadd_outy = 0.5;
    coord->imgtofile.add_outx = 0.0;
    coord->imgtofile.add_outy = 0.0;
    image->fiscaled = 0;
    /* update the transform for tracking info */
    combine_transform(&coord->buftofile, &coord->buftoimg, &coord->imgtofile);
    return( 0 );
  } else {
    /* copy the name */
    (void)strncpy(title, label, SZ_FNAME);
    label[SZ_FNAME - 1] = '\0';
    image->filename = title;
    /* install its coordinate transform matrix */
    coord->imgtofile.inx_outx = xx;
    coord->imgtofile.iny_outx = yx;
    coord->imgtofile.inx_outy = xy;
    coord->imgtofile.iny_outy = yy;
    coord->imgtofile.iadd_outx = xo;
    coord->imgtofile.iadd_outy = yo;
    /* fill in the missing add_out params */
    if( yx == 0.0 ) {
      coord->imgtofile.add_outx = xo - (0.5 * xx);
      coord->imgtofile.add_outy = yo - (0.5 * yy);
    } else {
      coord->imgtofile.add_outx = xo - (0.5 * yx);
      coord->imgtofile.add_outy = yo - (0.5 * xy);
    }
    /* apply any additional user requested rotation */
    if( (!image->row_order) || (image->rotate_code != 0) )
      rotate_transform(&coord->img, &coord->imgtofile,
		       !image->row_order, image->rotate_code);
    /* compute speedy integer computation parameters */
    set_trans_speed(&coord->imgtofile);
    /* compute the inverse parameters (complete) */
    invert_transform(&coord->filetoimg, &coord->imgtofile, 0.0);
    /* update the transform for tracking info */
    combine_transform(&coord->buftofile, &coord->buftoimg, &coord->imgtofile);
    /* check for subsection in title and compute true coords (if qp) */
    coord->imtool_aux = guess_true_file_coords(title);
    /* get IRAF/display scaling (uses linear scaling between low and high) */
    if( (tokens < 10) || (low == high) || (w_type != 1) ) {
      image->fiscaled = 0;
    } else {
      image->fiscaled = 1;
      image->fiscale = (high - low) / (CMS_DATARANGE - 1);
      image->fibias = low - image->fiscale;
    }
    show_filename();
    return( 1 );
  }
}

/*
 * Subroutine:	get_fbconfig
 * Purpose:	Read the IMTOOL startup file to get frame buffer sizes.
 * UNIX calls:	fopen(), fclose(), getenv()
 * File format:		configno nframes width height [extra fields]
 *		e.g.,		1  2  512  512
 *				2  2  800  800
 *				3  1 1024 1024		# comment
 */
int get_fbconfig ( config_number, width, height )
     int config_number;
     int *width, *height;
{
  register char	*ip;
  register FILE	*fp;
  int config, nframes;
  char lbuf[SZ_LINE+1], *fname, *getenv();

  /* 0 chooses the default configuration */
  if( config_number == 0 ) {
    *width = DEF_FRAME_WIDTH;
    *height = DEF_FRAME_HEIGHT;
    return( 1 );
  }
  /* attempt to open the config file. */
  fp = NULL;
  if( (fname=getenv(FBCONFIG_ENV1)) || (fname=getenv(FBCONFIG_ENV2)) )
    fp = fopen(fname, "r");
  if( !fp && (fname = getenv("HOME")) ) {
#ifndef VMS
    sprintf(lbuf, "%s/%s", fname, FBCONFIG_1);
#else
    sprintf(lbuf, "%s%s", fname, FBCONFIG_1);
#endif
    fp = fopen(fname = lbuf, "r");
  }
  if( !fp )
    fp = fopen(fname = FBCONFIG_2, "r");
  /* if cannot find a config file, return error */
  if( !fp ) {
    (void)fprintf(stderr, "Error: cannot find imtool configuration table\n");
    return( 0 );
  }
  /* scan the frame buffer configuration file. */
  do {
    /* read in next line and check for eof */
    if( fgets (lbuf, SZ_LINE, fp) == NULL ) {
      (void)fprintf(stderr,
		    "Error: configuration not found in %s table\n", fname);
      return( 0 );
    }
    config = 0;
    /* skip over leading spaces and tabs */
    for( ip=lbuf;  (*ip == ' ') || (*ip == '\t');  ip++ );
    /* skip comment lines and blank lines. */
    if( *ip == '\n' || *ip == '#' )
      continue;
    if( !isdigit(*ip) )
      continue;
    switch( sscanf (ip, "%d%d%d%d", &config, &nframes, width, height) ) {
    case 4:
      break;			/* normal case */
    case 3:
      *height = *width;		/* default to square format */
      break;
    default:
      (void)fprintf(stderr, "imtool: bad config `%s'\n", ip);
      config = 0;
      continue;
    }
    /* is this the configuration we want? */
  } while( config != config_number );
  if( *width < 1 ) *width = 1;
  if( *height < 1 ) *height = 1;
  (void)fclose(fp);
  return( 1 );
}
#endif
                    
                                                               
                                                               
                                                               
                                                               
                                                               
                                       
