#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnwpros.c (Region Write Pros)
 * Purpose:	Write regions to a file readable by the IRAF PROS system
 * Subroutine:	write_regions_pros()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  9 July 1989
 *		{1} MVH made boxes write diameter, not radius      7 Dec 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{3} MVH if center, point are int, write as int	  22 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, strcat, strcpy, rindex */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>		/* get X types and constants */
#include <X11/Xutil.h>		/* X window manager stuff, visuals */
#include "hfiles/color.h"	/* gcset needed by cursor */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/cursor.h"	/* define cursor parameter structures */
#include "hfiles/define.h"	/* define PI */

#define DEG(r) ((360.0 / TWO_PI) * (r))
#define RAD(d) ((TWO_PI / 360.0) * (d))

/*
 * Subroutine:	write_region_pros
 * Purpose:	Write ROSAT PROS style region description to file
 * Method:	Recurse first to start writing from end of link list
 */
void write_region_pros ( fd, region )
     FILE *fd;
     struct cursorRec *region;
{
  static void cat_annulus(), write_annuli(), cat_shape();

  /*  Check for pointer before doing anything  */
  if( region == NULL )
    return;
  /*  Recurse for higher regions so as to reverse order  */
  if( region->next_region != NULL )
    write_region_pros(fd, region->next_region);
  {
    /*  Declare locals here so as not to put them on stack while recursing  */
    char line[512];
    if( region->exclude_region )
      line[0] = '-';
    else
      line[0] = ' ';
    line[1] = '\0';
    if( region->annuli ) {
      if( region->type == COP_Circle ) {
	cat_annulus (region, line);
      } else {
	write_annuli(region, line, fd);
      }
    } else {
      cat_shape(region, line);
    }
    (void)strcat(line, "\n");
    /*  And write it out  */
    fputs(line, fd);
  }
}


/*  Subroutine:	cat_shape
 *  Purpose:	Put simple PROS description of given cursor in line buffer
 */     
static void cat_shape ( region, line )
     struct cursorRec *region;
     char *line;
{
  int params;
  void write_text_region();
  static void cat_polypts(), cat_cen(), cat_params();

  switch( region->type ) {
  case COP_Box:
    (void)strcat(line, "BOX(");
    if( region->rot.angle == 0.0 )
      params = 2;
    else
      params = 3;
    break;
  case COP_Circle:
    (void)strcat(line, "CIRCLE(");
    params = 1;
    break;
  case COP_Ellipse:
    (void)strcat(line, "ELLIPSE(");
    params = 3;
    break;
  case COP_Point:
    (void)strcat(line, "POINT(");
    params = 0;
    break;
  case COP_Polygon:
    if( region->poly_cnt >= 3 ) {
      (void)strcat(line, "POLYGON(");
      cat_polypts(region, line);
      (void)strcat(line, ")");
    }
    return;
  case COP_Arrow:
    /*  Make regionless cursor appear as a comment to most region parsers  */
    line[1] = line[0];
    line[0] = '#';
    (void)strcpy(&line[2], "ARROW(");
    params = 3;
    break;
  case COP_Text:
    /*  Make regionless cursor appear as a comment to most region parsers  */
    line[1] = line[0];
    line[0] = '#';
    (void)strcpy(&line[2], "TEXT(");
    cat_cen(region, line);
    write_text_region(region, line);
    return;
  default:
    return;
  }
  /*  Put it all together  */
  cat_cen(region, line);
  cat_params(region, line, params);
  (void)strcat(line, ")");
}


/*  Subroutine:	cat_params
 *  Purpose:	Add appropriate number of cursor shape parameters to
 *		line buffer
 */
static void cat_params ( region, line, count )
     struct cursorRec *region;
     char *line;
     int count;			/* i: number of shape parameters wanted */
{
  char params[32];
  switch( count ) {
  case 1:
    sprintf(params, ",%.2f", region->file.Xdim);
    break;
  case 2:
    if( region->type == COP_Box )
      /* PROS software expects boxes to use diameter instead of radius */
      sprintf(params, ",%.2f,%.2f",
	      region->file.Xdim * 2.0, region->file.Ydim * 2.0);
    else
      sprintf(params, ",%.2f,%.2f", region->file.Xdim, region->file.Ydim);
    break;
  case 3:
    if( region->type == COP_Box )
      sprintf(params, ",%.2f,%.2f,%.3f", region->file.Xdim * 2.0,
	      region->file.Ydim * 2.0, DEG(region->rot.angle));
    else
      sprintf(params, ",%.2f,%.2f,%.3f", region->file.Xdim,
	      region->file.Ydim, DEG(region->rot.angle));
    break;
  default:
    return;
  }
  (void)strcat(line, params);
}

/*
 * Subroutine:	cat_cen
 * Purpose:	Add center coordinates to line buffer
 */
static void cat_cen ( region, line )
     struct cursorRec *region;
     char *line;
{
  int ix, iy;
  char center[20];

  ix = (int)region->file.X;
  iy = (int)region->file.Y;
  /* print the center */
  if( ((double)ix == region->file.X) && ((double)iy == region->file.Y) )
    sprintf(center, "%d,%d", ix, iy);
  else
    sprintf(center, "%.2f,%.2f", region->file.X, region->file.Y);
  (void)strcat(line, center);
}

/*
 * Subroutine:	cat_polypts
 * Purpose:	Add list of point coordinates to line buffer (for polygon)
 */
static void cat_polypts ( region, line )
     struct cursorRec *region;
     char *line;
{
  int i;
  static void cat_pt();

  cat_pt((double)region->poly[0].fileX, (double)region->poly[0].fileY,
	 line, 0);
  for( i=1; i<region->poly_cnt; i++ ) {
    cat_pt((double)region->poly[i].fileX, (double)region->poly[i].fileY,
	   line, 1);
  }
}

/*
 * Subroutine:	cat_pt
 * Purpose:	Add given coordinates in parentheses to line buffer
 */
static void cat_pt ( x, y, line, comma )
     double x, y;
     char *line;
     int comma;
{
  int ix, iy;
  char point[20];

  ix = (int)x;
  iy = (int)y;
  /* print the point's coords */
  if( ((double)ix == x) && ((double)iy == y) ) {
    if( comma )
      sprintf(point, ",%d,%d", ix, iy);
    else
      sprintf(point, "%d,%d", ix, iy);
  } else {
    if( comma )
      sprintf(point, ",%.2f,%.2f", x, y);
    else
      sprintf(point, "%.2f,%.2f", x, y);
  }
  (void)strcat(line, point);
}

/*
 * Subroutine:	cat_annulus
 * Purpose:	Put a PROS style description of circular annuli on the
 *		line buffer
 */
static void cat_annulus ( region, line )
     struct cursorRec *region;
     char *line;
{
  char radius[16];
  static void cat_cen();

  (void)strcat(line, "ANNULUS(");
  cat_cen(region, line);
  /* first region is not one of the annuli */
  while( (region = region->next_annulus) != NULL ) {
    sprintf(radius, ",%.2f", region->file.Xdim);
    (void)strcat(line, radius);
  }
  (void)strcat(line, ")");
}

/*
 * Subroutine:	write_annuli
 * Purpose:	Write PROS type description to produce annuli of given shapes
 */
static void write_annuli ( region, line, fd )
     struct cursorRec *region;
     char *line;
     FILE *fd;
{
  struct cursorRec *annulus;
  static struct cursorRec *cat_annular();
  static void cat_shape();

  /* annuli start with next_annulus (base region is not one of them) */ 
  if( (annulus = region->next_annulus) != NULL ) {
    /* first line is the inner ring */
    cat_shape(annulus, line);
    (void)strcat(line, "\n");
    fputs(line, fd);
    line[0] = 0;
    while( annulus != NULL ) {
      annulus = cat_annular(annulus, line);
      /* write if there is a line, and it's not the last one */
      if( (annulus != NULL) && (annulus->next_annulus != NULL) ) {
	(void)strcat(line, "\n");
	/* and write it out */
	fputs(line, fd);
	line[0] = 0;
      }
    }
  }
}

/*
 * Subroutine:	cat_annular
 * Purpose:	Add outer shape anded with not of inner shape to make
 *		an annular ring
 */
static struct cursorRec *cat_annular ( region, line )
     struct cursorRec *region;
     char *line;
{
  static void cat_shape();

  if( region->next_annulus != NULL ) {
    if( region->exclude_region )
      line[0] = '-';
    else
      line[0] = ' ';
    line[1] = '\0';
    cat_shape(region->next_annulus, line);
    (void)strcat (line, " & !");
    cat_shape(region, line);
  }
  return( region->next_annulus );
}
