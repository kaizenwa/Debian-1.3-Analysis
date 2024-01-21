#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	irafcrd.c (IRAF Coordinates)
 * Purpose:	Parse wcs filename string for IRAF subsection coordinates
 * Subroutine:	guess_true_file_coords()	returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst				7 Nov 1989
 *		{1} MVH bug fix in guess_true_file_coords	7 Dec 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#ifdef IMTOOL

#include <stdio.h>		/* stderr, FILE, NULL, etc. */

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

#include <ctype.h>              /* isspace */
#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */

/*
 * Subroutine:	guess_true_file_coords
 * Purpose:	Parse wcs filename string for the user's "rect" subsection.
 *		If found and parsed, compute new transform for file coord.
 * Returns:	1 if subsection found and parsed successfully, else 0
 */
int guess_true_file_coords ( title )
     char *title;
{
  int xoff, yoff, block;
  float fblock;
  Transform imgtoaux;
  static int parse_iraf_subsection();
  void set_trans_speed(), combine_transform(), clear_coord_area();

  if( parse_iraf_subsection(title, &xoff, &yoff, &block) == 0 ) {
    if( coord.imtool_aux )
      clear_coord_area();
    return( 0 );
  }
  /* if this does nothing, don't bother */
  if( (xoff == 0) && (yoff == 0) && (block == 1) )
    return( 0 );
  /* install its coordinate transform matrix */
  fblock = (float)block;
  imgtoaux.inx_outx = coord.imgtofile.inx_outx * fblock;
  imgtoaux.iny_outx = coord.imgtofile.iny_outx * fblock;
  imgtoaux.inx_outy = coord.imgtofile.inx_outy * fblock;
  imgtoaux.iny_outy = coord.imgtofile.iny_outy * fblock;
  imgtoaux.iadd_outx = (xoff - 1) +
    ((fblock * coord.imgtofile.iadd_outx) - ((fblock - 1) * 0.5));
  imgtoaux.iadd_outy = (yoff - 1) +
    ((fblock * coord.imgtofile.iadd_outy) - ((fblock - 1) * 0.5));
  /* fill in the missing add_out params */
  if( imgtoaux.iny_outx == 0.0 ) {
    imgtoaux.add_outx = imgtoaux.iadd_outx - (0.5 * imgtoaux.inx_outx);
    imgtoaux.add_outy = imgtoaux.iadd_outy - (0.5 * imgtoaux.iny_outy);
  } else {
    imgtoaux.add_outx = imgtoaux.iadd_outx - (0.5 * imgtoaux.iny_outx);
    imgtoaux.add_outy = imgtoaux.iadd_outy - (0.5 * imgtoaux.inx_outy);
  }
  /* compute speedy integer computation parameters */
  set_trans_speed(&imgtoaux);
  /* compute transform from file to aux (by going file to img to aux) */
  combine_transform(&coord.filetoaux, &coord.filetoimg, &imgtoaux);
  return( 1 );
}

/*
 * Subroutine:	parse_iraf_subsection
 * Purpose:	Parse for subsection and blocking from the image name
 * Returns:	1 if subsection or blocking was used, else 0
 */
static int parse_iraf_subsection ( s, xoff, yoff, block )
     char *s;			/* i: file name */
     int *xoff;			/* o: x offset */
     int *yoff;			/* o: y offset */
     int *block;		/* o: block */
{
  char *t, *u;			/* temp char pointers */
  static char *fn_substr();	/* look for a substr */
  static void get_subsection_offsets();

  /* seed the default values */
  *xoff = 1;
  *yoff = 1;
  *block=1;
  /* look for a left bracket */
  t = fn_substr(s, "[");
  /* no bracket => no offsets or block */
  if( t ==0 )
    return( 0 );
  /* check for qp file identifier */
  u = fn_substr(s, ".qp");
  if( (u !=0) && (u < t) ) {
    /* look for the rect substring */
    u = fn_substr(t, "rect=(" );
    /* if we find rect, grab the initial x offset */
    if( u !=0 )
      get_subsection_offsets (u, xoff, yoff, 1);
    /* look for the block substring */
    u = fn_substr(t, "block=" );
    /* if we find the block, grab the block factor */
    if( u !=0 ) {
      /* might have to skip '(' */
      while( *u == '(' )
	*u++;
      *block = atoi(u);
    }
  } else {
    /* iraf sends subsection coords for qp with "rect=", else file coords */
    return( 0 );
  }
  return( 1 );
}

/*
 * Subroutine:	get_subsection_offsets
 * Purpose:	parse subsection syntax for x and y offsets of subsection
 */
static void get_subsection_offsets ( s, xoff, yoff, is_qp )
     char *s;
     int *xoff, *yoff;
     int is_qp;		/* i: file is qp (uses () around subsection) */
{
  int x1, x2;
  int y1, y2;
  char *sy, *s2, *st;
  static char *fn_substr();

  /* if not qp, grab the initial x offset right after the '[' */
  sy = fn_substr(s, ",");
  if( is_qp )
    st = strchr(s, ')');
  else
    st = strchr(s, ']');
  if( (x1 = atoi(s)) > 1 ) {
    s2 = fn_substr(s, ":");
    /* make sure ':' was found and isn't that for y coords */
    if( (s2 != NULL) && ((sy == NULL) || (s2 < sy)) ) {
      x2 = atoi(s2);
      if( (x2 <= 0) || (x2 > x1) )
	*xoff = x1;
      else
	/* giving lower x2 reverses column order, offset is from x2 */
	*xoff = x2;
    } else
      *xoff = x1;
  }
  /* if there was a comma and a y subsection given */
  if( (sy != NULL) && (sy < st) && ((y1 = atoi(sy)) > 1) ) {
    s2 = fn_substr(sy, ":");
    /* make sure ':' was found and is for the subsection */
    if( (s2 != NULL) && (s2 < st) ) {
      y2 = atoi(s2);
      if( (y2 <= 0) || (y2 > y1) )
	*yoff = y1;
      else
	*yoff = y2;
    } else
      *yoff = y1;
  }
  if( *xoff <= 0 )
    *xoff = 1;
  if( *yoff <= 0 )
    *yoff = 1;
}

/*
 * Subroutine:	fn_substr
 * Purpose:	Find the next instance of a string in another string 
 * Note:	This routine is a bit different in that it ignores spaces
 *		in checking and returns a pointer after the substring
 */
static char *fn_substr ( s, p )
     char *s;	/* i: string to be searched */
     char *p;	/* i: pattern sought in s */
{
  char *sl;	/* pointer to the local part of s */
  char *tp;	/* local pointer in p */
  char *sp;	/* local pointer in s */

  /* if the pattern we are searching is null, no sense going on */
  if( *p == '\0' )
    return( NULL );
  sl = s;
  /* look through the mother string */
  while( (sl = strchr(sl,*p)) != 0 )
    {
      for( sp=sl,tp=p; (*tp)&&((*sp == *tp)||(isspace(*sp))); sp++ )
	/* if we found a space, we can't bump the t pointer */
	if( !isspace(*sp) )
	   tp++;
      if( *tp == '\0' )
	return(sp);  /* found one */
      sl++;  /* advance a character */
    }
  return( NULL );
}

#endif
