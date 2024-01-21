#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnmake.c (Region Make)
 * Purpose:	Create cursor regions given sets of parameters
 * Subroutine:	make_next_region()		returns: void
 * Xlib calls:	none
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  11 May 1989
 *		{1} MVH box dims are read as diameters		   7 Dec 1989
 *		{2} MVH support for text and arrow label cursors   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#else
#include <strings.h>		/*  strlen, strcat, strcpy, rindex  */
#define strchr index
#endif
#else
#include <string.h>		/*  strlen, strcat, strcpy, strrchr  */
#endif

#include <stdio.h>		/* get stderr, define sscanf */
#include <math.h>		/* sin, cos, etc */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/define.h"	/* TWO_PI */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main SAOimage parameter structures */


/*  Amount of space for string is actually (CURSOR_MAX - 1)*sizeof(XPoint)  */
struct curText {
  int char_cnt;
  char string[CURSOR_MAX*2];
};


#ifdef ANSIC

void	    make_next_region(	int type, char *line, int exclude,
				int draw, float *scratch, int scratch_max);
static void parse_error(	char* line);
static void make_annuli(	struct cursorRec *region, char *line,
				float *radii, int radii_max);
void	    make_polygon(	char *line, int exclude, int draw,
				float *vertex_coord, int coord_max);
void	    make_polygon(	char *line, int exclude, int draw,
				float *vertex_coord, int coord_max);
static int  parse_coords(	char *line, float *coord, int maxcnt);

#else

  static void make_annuli(), parse_error();
  static int parse_coords();
  struct cursorRec *get_new_cursor();
  char *next_token();
  void set_cursor_from_file_coords(), disp_cursor();
  void make_polygon(), free_cursor();
  int parse_radii();
  void set_cursor_from_file_coords(), make_new_annulus();
  void set_polygon_from_file_coords();

#endif


#define DEG(r) ((360.0 / TWO_PI) * (r))
#define RAD(d) ((TWO_PI / 360.0) * (d))

extern int CircleCnt;
extern int EllipseCnt;


/*  Subroutine:	make_next_region
 *  Purpose:	Make and install a region given basic file coord parameters
 */
#ifdef ANSIC
void make_next_region( int type, char *line, int exclude,
		       int draw, float *scratch, int scratch_max )
#else
void make_next_region( type, line, exclude, draw, scratch, scratch_max )
     int type;		/* i: cursor type */
     char *line;	/* i: ascii string with parameters */
     int exclude;	/* i: region-is-exclude_region */
     int draw;		/* i: draw-region-after-making-it */
     float *scratch;	/* i: scratch space for list of floats */
     int scratch_max;	/* i: size of scratch space */
#endif
{
  struct cursorRec *region;
  float angle, tempX, tempY;
  int point_cnt, tokens, char_cnt;
  int annuli = 0;

  switch( type ) {
  case COP_Point:
    point_cnt = 0;
    break;
  case COP_Box:
    point_cnt = 5;
    break;
  case COP_Annuli:
    annuli = 1;
    type = COP_Circle;
  case COP_Circle:
    point_cnt = CircleCnt + 1;
    break;
  case COP_Ellipse:
    point_cnt = EllipseCnt + 1;
    break;
  case COP_Arrow:
    point_cnt = 6;
    break;
  case COP_Text:
    if( 3 != sscanf(line, "%f%f%d", &tempX, &tempY, &char_cnt) ) {
      parse_error(line);
      return;
    }
    point_cnt = ((char_cnt + 1) / sizeof(XPoint)) + 2;
    break;
  case COP_Polygon:
    make_polygon(line, exclude, draw, scratch, scratch_max);
  default:
    return;
  }
  region = get_new_cursor(point_cnt, 0);
  region->type = type;
  region->point_cnt = point_cnt;
  if( (region->next_region = cursor.next_region) != NULL )
    region->index = cursor.next_region->index + 1;
  else
    region->index = 1;
  cursor.next_region = region;
  /*  Put in text cursor info  */
  if( type == COP_Text ) {
    struct curText *curtext = (struct curText *)cursor.next_region->points;
    curtext->char_cnt = char_cnt;
    line = strchr(line, '\"');
    (void)strncpy(curtext->string, &line[1], char_cnt);
    region->file.X = tempX;
    region->file.Y = tempY;
  } else {
    tokens = sscanf(line, "%f%f%f%f%f", &region->file.X, &region->file.Y,
		    &region->file.Xdim, &region->file.Ydim, &angle);
    if( type == COP_Box ) {
      /*  Boxes in PROS use diameter, internally they use radius  */
      region->file.Xdim *= 0.5;
      region->file.Ydim *= 0.5;
    }
    if( (tokens > 4) && (type != COP_Circle) && (type != COP_Point) ) {
      region->rot.angle = RAD((double)angle);
      region->rot.cos = cos(PI - region->rot.angle);
      region->rot.sin = sin(PI - region->rot.angle);
    } else {
      region->rot.angle = 0.0;
      region->rot.cos = -1.0;
      region->rot.sin = 0.0;
      if( (tokens < 3) || (type == COP_Point) ) {
	if( type != COP_Point ) {
	  parse_error(line);
	  cursor.next_region = region->next_region;
	  free_cursor (region);
	  return;
	} else {
	  region->file.Xdim = 0.0;
	  /*  Recurse on a list of points  */
	  if( tokens >= 4 ) {
	    /*  Advance to next coordinate pair  */
	    line = next_token (line, 2);
	    make_next_region(type, line, exclude, draw, scratch, scratch_max);
	  }
	}
	if( (tokens < 4) || (type == COP_Circle) )
	  region->file.Ydim = region->file.Xdim;
      }
    }
  }
  if( (region->exclude_region = exclude) )
    region->draw = &color.gcset.excl;
  else
    region->draw = &color.gcset.incl;
  region->win.display = dispbox.display;
  region->win.ID = dispbox.ID;
  region->overwrites_image_data = 1;
  /* in case of circular annuli, this was just the beginning */
  if( annuli ) {
    region->annuli = 1;
    make_annuli(region, line, scratch, scratch_max);
  } else {
    set_cursor_from_file_coords(region, &coord.filetodisp);
  }
  if( draw )
    disp_cursor(region);
}


/*  Subroutine:	parse_error
 *  Purpose:	Print parsing error message
 */
#ifdef ANSIC
static void parse_error( char* line )
#else
static void parse_error( line )
  char* line;
#endif
{
  (void)fprintf(stderr, "Parse error: too few parameters parsed\n");
  (void)fprintf(stderr, "-> %s\n", line);
}


/*  Subroutine:	make_annuli
 *  Purpose:	Make circular annuli given base region and string of radii
 *  Note:	Base region is circle of first radius
 *  Method:	Make base region as each ring, then copy as one of its annuli
 *		(mimics procedure user would take).
 *  Called by:	make_region() above
 */
#ifdef ANSIC
static void make_annuli ( struct cursorRec *region, char *line,
			  float *radii, int radii_max )
#else
static void make_annuli ( region, line, radii, radii_max )
     struct cursorRec *region;
     char *line;
     float *radii;
     int radii_max;
#endif
{
  int i, cnt;

  /*  Advance beyond center coords  */
  line = next_token(line, 2);
  cnt = parse_radii(line, radii, radii_max);
  /*  Reverse order for ascending insert sort from list in ascending order  */
  for( i = cnt - 1; i >= 0; i-- ) {
    region->file.Xdim = radii[i];
    region->file.Ydim = region->file.Xdim;
    set_cursor_from_file_coords(region, &coord.filetodisp);
    make_new_annulus(region);
  }
}


/*  Subroutine:	parse_coords
 *  Purpose:	Parse coordinate pairs off the line
 *  Called by:	make_polygon() below
 */
#ifdef ANSIC
static int parse_coords ( char *line, float *coord, int maxcnt )
#else
static int parse_coords ( line, coord, maxcnt )
     char *line;
     float *coord;
     int maxcnt;
#endif
{
  int i;

  /*  Keep reading while there are tokens on the line  */
  for( i=0; (line != NULL) && (i < maxcnt); i+= 2 ) {
    if( sscanf(line, "%f%f", &coord[i], &coord[i+1]) != 2 )
      return( 0 );
    line = next_token(line, 2);
  }
  return( i );
}


/*  Subroutine:	make_polygon
 *  Purpose:	Parse line and make a polygon region
 */
#ifdef ANSIC
void make_polygon ( char *line, int exclude, int draw, float *vertex_coord,
		    int coord_max )
#else
void make_polygon ( line, exclude, draw, vertex_coord, coord_max )
     char *line;
     int exclude;
     int draw;
     float *vertex_coord;
     int coord_max;
#endif
{
  int i, j;
  int cnt;
  struct cursorRec *region;

  cnt = parse_coords(line, vertex_coord, coord_max);
  if( cnt % 2 ) {
    (void)fprintf(stderr, "Parse error: odd number for pairs: %d\n", cnt);
    (void)fprintf(stderr, "-> %s\n", line);
    return;
  }
  cnt /= 2;
  region = get_new_cursor(cnt + 1, cnt);
  region->type = COP_Polygon;
  region->poly_cnt = cnt;
  region->point_cnt = cnt + 1;
  for( j=0, i=0; i<cnt; i++ ) {
    region->poly[i].fileX = vertex_coord[j++];
    region->poly[i].fileY = vertex_coord[j++];
  }
  region->file.X = vertex_coord[0];
  region->file.Y = vertex_coord[1];
  set_polygon_from_file_coords(region, &coord.filetodisp, 0);
  if( (region->next_region = cursor.next_region) != NULL )
    region->index = cursor.next_region->index + 1;
  else
    region->index = 1;
  cursor.next_region = region;
  if( (region->exclude_region = exclude) )
    region->draw = &color.gcset.excl;
  else
    region->draw = &color.gcset.incl;
  region->win.display = dispbox.display;
  region->win.ID = dispbox.ID;
  region->overwrites_image_data = 1;
  if( draw )
    disp_cursor(region);
}
