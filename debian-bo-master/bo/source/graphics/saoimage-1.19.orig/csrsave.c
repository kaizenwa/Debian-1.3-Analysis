#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrsave.c (Cursor Save)
 * Purpose:	Make new copies of the cursor or replace the cursor by copying
 * Subroutine:	get_new_cursor()		returns: struct cursorRec *
 * Subroutine:	free_cursor()			returns: void
 * Subroutine:	copy_cursor()			returns: struct cursorRec *
 * Subroutine:	save_cursor_as_region()		returns: void
 * Subroutine:	copy_region_to_cursor()		returns: void
 * Subroutine:	match_region()			returns: int
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{1} MVH support for Arrow cursor		   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/cursor.h"	/* define cursor parameter structures */

extern struct colorRec color;	/* need to know color.gcset */

#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

struct cursorRec *get_new_cursor(	int point_cnt, int poly_cnt);
void		free_cursor(		struct cursorRec *cursor);
struct cursorRec *copy_cursor(		struct cursorRec *cursor);
void		save_cursor_as_region(	struct cursorRec *cursor, int exclude);
void		copy_region_to_cursor(	struct cursorRec *cursor,
					struct cursorRec *region);
int		match_region(		struct cursorRec *cursor,
					struct cursorRec *region);

static struct cursorRec *copy_annulus(	struct cursorRec *annulus,
				        int exclude);

#else

  char *calloc_errchk();
  struct cursorRec *get_new_cursor();
  double cursor_area();
  void copy_polygon_region_to_cursor();
  static struct cursorRec *copy_annulus();
  struct cursorRec *copy_cursor();

#endif


/*  Subroutine:	get_new_cursor()
 *  Returns:	Returns space for cursor including points
 *  Note:	Space is alloc'd as single chunk and can be freed en mass
 */
#ifdef ANSIC
struct cursorRec *get_new_cursor ( int point_cnt, int poly_cnt )
#else
struct cursorRec *get_new_cursor ( point_cnt, poly_cnt )
     int point_cnt;	/* i: number of drawing points needed */
     int poly_cnt;	/* i: number of polygon vertex records needed */
#endif
{
  struct cursorRec *cursor;
  int size;

  /*  Convert segment count to point count (for Arrow Cursor)  */
  if( point_cnt < 0 )
    point_cnt *= -2;
  /*  Don't take any chances on alignment, use multiples of 4  */
  if( (size = point_cnt % 4) )
    point_cnt += 4 - size;
  size = sizeof(struct cursorRec) +
    (point_cnt * sizeof(XPoint)) + (poly_cnt * sizeof(PolyPoint));
  cursor = (struct cursorRec *)calloc_errchk(1, (unsigned int)size, "cursor");
  if( point_cnt )
    cursor->points = (XPoint *)&(cursor[1]);
  if( poly_cnt )
    cursor->poly = (PolyPoint *)&(cursor->points[point_cnt]);
  /*  Set the default angle at 0.0 degrees, cos must be non-zero  */
  cursor->rot.cos = -1.0;
  return( cursor );
}


/*  Subroutine:	free_cursor
 *  Purpose:	Free alloc'd cursor space
 */
#ifdef ANSIC
void free_cursor ( struct cursorRec *cursor )
#else
void free_cursor ( cursor )
     struct cursorRec *cursor;
#endif
{
  free((char *)cursor);
}


/*  Subroutine:	copy_cursor
 *  Returns:	A copy of the cursor in a newly allocated cursor record
 */
#ifdef ANSIC
struct cursorRec *copy_cursor ( struct cursorRec *cursor )
#else
struct cursorRec *copy_cursor ( cursor )
     struct cursorRec *cursor;
#endif
{
  struct cursorRec *copy;
  XPoint *points;
  PolyPoint *poly;

  /*  Get a new cursor space  */
  copy = get_new_cursor(cursor->point_cnt, cursor->poly_cnt);
  points = copy->points;
  poly = copy->poly;
  /*  Copy the contents of cursor to copy  */
#ifdef ANSIC
  (void)memcpy((void *)copy, (void *)cursor, sizeof(struct cursorRec));
  copy->points = points;
  copy->poly = poly;
  if( cursor->point_cnt > 0 )
    (void)memcpy((void *)points, (void *)cursor->points,
		 cursor->point_cnt * sizeof(XPoint));
  else if( cursor->point_cnt < 0 )
    (void)memcpy((void *)points, (void *)cursor->points,
		 (size_t)(cursor->point_cnt * (-sizeof(XSegment))));
  if( cursor->poly_cnt )
    (void)memcpy((void *)poly, (void *)cursor->poly,
		 cursor->poly_cnt * sizeof(PolyPoint));
#else
  bcopy((char *)cursor, (char *)copy, sizeof(struct cursorRec));
  copy->points = points;
  copy->poly = poly;
  if( cursor->point_cnt > 0 )
    bcopy((char *)cursor->points, (char *)points,
	  cursor->point_cnt * sizeof(XPoint));
  else if( cursor->point_cnt < 0 )
    bcopy((char *)cursor->points, (char *)points,
	  -cursor->point_cnt * sizeof(XSegment));
  if( cursor->poly_cnt )
    bcopy((char *)cursor->poly, (char *)poly,
	  cursor->poly_cnt * sizeof(PolyPoint));
#endif
  /*  Rectangles are not included  */
  copy->rectangles = NULL;
  copy->rectangle_cnt = 0;
  /*  Install cursor's area  */
  copy->file.area = cursor_area(cursor, 0);
  return( copy );
}


/*  Subroutine:	save_cursor_as_region
 *  Purpose:	Copy the cursor onto a region of type include or exclude
 */
#ifdef ANSIC
void save_cursor_as_region ( struct cursorRec *cursor, int exclude )
#else
void save_cursor_as_region ( cursor, exclude )
     struct cursorRec *cursor;
     int exclude;		/* i: include or exclude */
#endif
{
  struct cursorRec *region;

  /*  Create and fill new cursor structure  */
  region = copy_cursor(cursor);
  /* set up regions drawing characteristics */
  if( (region->exclude_region = exclude) )
    region->draw = &color.gcset.excl;
  else
    region->draw = &color.gcset.incl;
  region->overwrites_image_data = 1;
  /*  Attach to cursor link list  */
  if( cursor->next_region == 0 )
    region->index = 1;
  else
    region->index = cursor->next_region->index + 1;
  region->next_region = cursor->next_region;
  cursor->next_region = region;
  /*  Install annuli if any  */
  if( cursor->annuli && (cursor->next_annulus != NULL) ) {
    region->next_annulus = copy_annulus(cursor->next_annulus, exclude);
  } else
    region->next_annulus = NULL;
}


/*  Subroutine:	copy_annulus
 *  Purpose:	Reconstruct annulus structure for copy
 *  Returns:	Complete copy of the annulus chain given
 *  Method:	Given an annulus chain, copy this link, add a copy of the
 *		rest of the chain (by recursion) and return the copy
 */
#ifdef ANSIC
static struct cursorRec *copy_annulus ( struct cursorRec *annulus,
				        int exclude )
#else
static struct cursorRec *copy_annulus ( annulus, exclude )
     struct cursorRec *annulus;
     int exclude;
#endif
{
  struct cursorRec *region;

  region = copy_cursor(annulus);
  region->exclude_region = exclude;
  if( annulus->next_annulus != NULL )
    region->next_annulus = copy_annulus(annulus->next_annulus, exclude);
  return( region );
}


/*  Subroutine:	copy_region_to_cursor
 *  Purpose:	Copy region onto cursor
 */
#ifdef ANSIC
void copy_region_to_cursor ( struct cursorRec *cursor,
			     struct cursorRec *region )
#else
void copy_region_to_cursor ( cursor, region )
     struct cursorRec *cursor;
     struct cursorRec *region;
#endif
{
  int overwrites;			/* cursor overwrites image data */
  GCspec *draw;
  XPoint *points;
  struct cursorRec *next_region;

  /*  Save things which belong to the cursor and not a region  */
  overwrites = cursor->overwrites_image_data;
  draw = cursor->draw;
  next_region = cursor->next_region;
  points = cursor->points;
  /*  Copy the contents of region to cursor  */
#ifdef ANSIC
  (void)memcpy((void *)cursor, (void *)region, sizeof(struct cursorRec));
  if( region->point_cnt > 0 )
    (void)memcpy((void *)points, (void *)region_points,
		 (size_t)region->point_cnt * sizeof(XPoint));
  else if( region->point_cnt < 0 )
    (void)memcpy((void *)points, (void *)region_points,
		 (size_t)(region->point_cnt * (-sizeof(XSegment)));
#else
  bcopy((char *)region, (char *)cursor, sizeof(struct cursorRec));
  if( region->point_cnt > 0 )
    bcopy((char *)region->points, (char *)points,
	  region->point_cnt * sizeof(XPoint));
  else if( region->point_cnt < 0 )
    bcopy((char *)region->points, (char *)points,
	  -region->point_cnt * sizeof(XSegment));
#endif
  if( region->annuli && (region->next_annulus != NULL) )
    cursor->next_annulus = copy_annulus (region->next_annulus, 1);
  /*  Restore things which were uniquely the cursor's  */
  cursor->overwrites_image_data = overwrites;
  cursor->draw = draw;
  cursor->next_region = next_region;
  cursor->points = points;
  /*  Set up drawing for polygon or for other cursor types  */
  if( cursor->type == COP_Polygon ) {
    copy_polygon_region_to_cursor (region, cursor);
  }
}


/*  Subroutine:	match_region
 *  Purpose:	Compare a region with a cursor
 *  Returns:	0 if perfect match, else 1
 */
#ifdef ANSIC
int match_region ( struct cursorRec *cursor, struct cursorRec *region )
#else
int match_region ( cursor, region )
     struct cursorRec *cursor, *region;
#endif
{
  int i;

  if( (region != NULL) && (cursor != NULL) &&
      (region->type == cursor->type) ) {
    if( region->type == COP_Polygon ) {
      if( region->poly_cnt == cursor->poly_cnt ) {
	for( i=0; i<region->poly_cnt; i++ ) {
	  if( (region->points[i].x != cursor->points[i].x) ||
	      (region->points[i].y != cursor->points[i].y) )
	    return( 0 );
	}
	return( 1 );
      }
    } else if( region->annuli ) {
      cursor = cursor->next_annulus;
      region = region->next_annulus;
      while( (cursor != NULL) && (region != NULL ) ) {
	if( (region->win.X != cursor->win.X) ||
	    (region->win.Y != cursor->win.Y) ||
	    (region->win.rayX != cursor->win.rayX) ||
	    (region->win.rayY != cursor->win.rayY) )
	  return( 0 );
	region = region->next_annulus;
	cursor = cursor->next_annulus;
      }
      if( region == cursor )
	return( 1 );
    } else {
      if( (region->win.X == cursor->win.X) &&
	  (region->win.Y == cursor->win.Y) &&
	  (region->win.rayX == cursor->win.rayX) &&
	  (region->win.rayY == cursor->win.rayY) )
	return( 1 );
    }
  }
  return( 0 );
}
