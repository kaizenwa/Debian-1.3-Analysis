#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgndraw.c (Region Draw)
 * Purpose:	Draw regions and their labels
 * Subroutine:	init_region_draw()		returns: void
 * Subroutine:	toggle_region_visibility()	returns: void
 * Subroutine:	toggle_region_labeling()	returns: void
 * Subroutine:	disp_regions()			returns: void
 * Subroutine:	disp_region()			returns: void
 * Subroutine:	label_region_cycle_magnifier()	returns: void
 * Xlib calls:	XDrawImageString(), XDrawString(), XDrawLine(), XTextWidth()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  26 May 1989
 *              {1} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* define stderr, NULL */

#ifndef VMS
#ifdef SYSV
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#else
#include <strings.h>		/* strlen, etc. for unenlightened BSD's */
#endif
#else
#include <string.h>		/* strlen, strcat, strcpy, strrchr */
#endif

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/define.h"	/* define MIN, MAX, U_DONT_CARE, etc. */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/struct.h"	/* declare structure types */
#include "hfiles/extern.h"	/* extern main parameter structures */
#include "hfiles/region.h"	/* regdrawRec region drawing parameters */

struct regdrawRec rgdraw;

/*
 * Subroutine:	init_region_draw
 */
void init_region_draw ( view, label )
{
  XFontStruct *get_fontstruct();
  void set_submenu_toggle();

  /* set button panel buttons */
  if( (rgdraw.visible = view) )
    set_submenu_toggle (ROP, ROP_View, 1);
  if( (rgdraw.label = label) )
    set_submenu_toggle (ROP, ROP_Label, 1);
  /* get the medium font for legible numbers but limited size */
  rgdraw.fontstruct = get_fontstruct(1);
  rgdraw.label_xoff = 1;
  if( rgdraw.filled_label ) {
    rgdraw.label_yoff = -rgdraw.fontstruct->descent;
    rgdraw.label_height =
      rgdraw.fontstruct->descent + rgdraw.fontstruct->ascent;
  } else {
    rgdraw.label_yoff = 0;
    rgdraw.label_height = rgdraw.fontstruct->ascent + 1;
  }
}

/*
 * Subroutine:	toggle_region_visibility
 */
void toggle_region_visibility ( )
{
  void disp_dispbox();

  rgdraw.visible ^= 1;
  /* redraw the image, (it will call disp_regions) */
  disp_dispbox();
}

/*
 * Subroutine:	toggle_region_labeling
 */
void toggle_region_labeling ( )
{
  void disp_dispbox();

  rgdraw.label ^= 1;
  /* redraw the image, (it will call disp_regions) */
  disp_dispbox();
}

/*
 * Subroutine:	disp_regions
 * Purpose:	Draw all saved cursor regions
 */
void disp_regions ( cursor )
     struct cursorRec *cursor;
{
  struct cursorRec *region;
  void disp_region();

  if( rgdraw.visible ) {
    region = cursor->next_region;
    while( region != NULL ) {
      disp_region (region);
      region = region->next_region;
    }
  }
}

/*
 * Subroutine:	disp_region
 * Purpose:	Draw one saved cursor region and its labels
 */
void disp_region ( region )
     struct cursorRec *region;
{
  GC gc;
  static GC set_region_gc();
  static void label_annuli(), label_region(), draw_region();

  if( rgdraw.visible && (region != NULL) ) {
    gc = set_region_gc(region);
    draw_region(region, gc);
    if( rgdraw.label ) {
      if( region->annuli )
	label_annuli(region, gc);
      else
	label_region(region, gc);
    }
  }
}

/*
 * Subroutine:	label_region_cycle_magnifier
 * Purpose:	Label cycle zoombox display
 * Xlib calls:	XDrawImageString(), XTextWidth()
 */
void label_region_cycle_magnifier ( region, do_index )
     struct cursorRec *region;
     int do_index;
{
  int text_y, index_x, center_x;
  char index[4], center[32];
  GC gc, set_edit_gc();

  /* make center label (round to correct integer pixel coordinate) */
  if( img.index_base )
    (void)sprintf(center, "(%d,%d)", RND(region->file.X), RND(region->file.Y));
  else
    (void)sprintf(center, "(%d,%d)", (int)region->file.X, (int)region->file.Y);
  text_y = magnibox.height -
    (rgdraw.fontstruct->ascent + rgdraw.fontstruct->descent);
  index_x = rgdraw.fontstruct->max_bounds.rbearing;
  center_x = magnibox.width -
    (XTextWidth(rgdraw.fontstruct, center, strlen(center)) + index_x);
  gc = set_edit_gc(rgdraw.fontstruct->fid,
		   region->draw->foreground, region->draw->background);
  XDrawImageString(magnibox.display, magnibox.ID, gc,
		   center_x, text_y, center, strlen(center));
  if( do_index ) {
    (void)sprintf(index, " %d ", region->index);
    XDrawImageString(magnibox.display, magnibox.ID, gc,
		     index_x, text_y, index, strlen(index));
  }
}

/*
 * Subroutine:	set_region_gc
 * Purpose:	Set up the gc with only as much detail as needed
 */
static GC set_region_gc ( region )
     struct cursorRec *region;
{
  GC gc, set_edit_gc(), set_text_gc(), set_gc();

  if( rgdraw.label ) {
    if( rgdraw.filled_label )
      gc = set_edit_gc(rgdraw.fontstruct->fid,
		       region->draw->foreground, region->draw->background);
    else
      gc = set_text_gc(rgdraw.fontstruct->fid, region->draw->foreground,
		       U_DONT_CARE, GXcopy, AllPlanes);
  } else
    gc = set_gc(region->draw);
  return( gc );
}

/*
 * Subroutine:	draw_region_label
 * Xlib calls:	XDrawImageString(), XDrawString()
 */
static void draw_region_label ( string, cnt, gc, x, y )
     char *string;
     int cnt;
     GC gc;
     int x, y;
{
  if( (x > 0) &&
      (x <= (dispbox.width - rgdraw.fontstruct->max_bounds.rbearing)) &&
      (y >= rgdraw.fontstruct->max_bounds.ascent) &&
      (y <= (dispbox.height - rgdraw.fontstruct->max_bounds.descent)) ) {
    if( rgdraw.filled_label )
      XDrawImageString(dispbox.display, dispbox.ID, gc, x, y, string, cnt);
    else
      XDrawString(dispbox.display, dispbox.ID, gc, x, y, string, cnt);
  }
}

/*
 * Subroutine:	draw_region
 * Purpose:	Draw one saved cursor region outline or point index
 * Note:	gc is already set, hence NULL GCspec in draw calls
 */
static void draw_region ( region, gc )
     struct cursorRec *region;
     GC gc;
{
  static void draw_region_label();
  void draw_annuli(), draw_cursor();

  if( region->type != COP_Point ) {
    if( region->annuli )
      /* if there are annuli, draw them (GC is already set) */
      draw_annuli(region, (GCspec *)NULL);
    else
      draw_cursor(region, (GCspec *)NULL);
  } else {
#ifdef LBLPNT
    if( !rgdraw.label ) {
      char label[32];

      sprintf(label, "%d", region->index);
      draw_region_label(label, strlen(label), gc,
			region->win.x + rgdraw.label_xoff,
			region->win.y + rgdraw.label_yoff);
    }
#endif
    XDrawPoint(dispbox.display, dispbox.ID, gc, region->win.x, region->win.y);
  }
}

/*
 * Subroutine:	label_annuli
 */
static void label_annuli ( region, gc )
     struct cursorRec *region;
     GC gc;
{
  struct cursorRec *annulus;
  static void label_region();

  /* apply label to outer annulus */
  annulus = region;
  while( annulus->next_annulus != NULL )
    annulus = annulus->next_annulus;
  annulus->index = region->index;
  annulus->draw = region->draw;
  annulus->exclude_region = region->exclude_region;
  label_region(annulus, gc);
}

/*
 * Subroutine:	label_region
 * Purpose:	Draw one region's label line and text for center and edge
 * Xlib calls:	XDrawLine()
 */
static void label_region ( region, gc )
     struct cursorRec *region;
     GC gc;
{
  int x0, y0, x1, y1, yt;
  char center_string[16];
  char edge_string[132];
  int len;
  static int mark_include();
  static void draw_region_label();

  /* if label not requested or don't know how to label or will another part */
  if( (!rgdraw.label) || (region->type == COP_Polygon) )
    return;
  /* make center label (round to correct integer pixel coordinate) */
  if( img.index_base )
    sprintf(center_string, "(%d,%d)",
		  RND(region->file.X), RND(region->file.Y));
  else
    sprintf(center_string, "(%d,%d)",
		  (int)region->file.X,(int)region->file.Y);
  x0 = region->win.x;
  y0 = region->win.y;
  if( region->type == COP_Point ) {
    len = mark_include(center_string, region->exclude_region);
    draw_region_label(center_string, len, gc,
		      x0 + rgdraw.label_xoff, y0 + rgdraw.label_yoff);
  } else {
    /* prepare and draw a line between center and edge of region */
    switch( region->type ) {
    case COP_Circle:
      x1 = region->win.x;
      y1 = region->win.y - (int)(region->win.rayX + 0.5);
      sprintf(edge_string, "%d", (int)(region->file.Xdim + 0.5));
      break;
    case COP_Box:
    case COP_Ellipse:
      if( region->rot.angle == 0.0 ) {
	x1 = region->win.x;
	y1 = region->win.y - (int)(region->win.rayY + 0.5);
      } else {
	/* transform back to unrotated coords */
	x1 = region->win.x - RND(region->win.rayY * region->rot.sin);
	y1 = region->win.y + RND(region->win.rayY * region->rot.cos);
      }
      sprintf(edge_string, "%d", (int)(region->file.Ydim + 0.5));
      break;
    default:
      return;
    }
    /* place text at end of line, but not over other label */
    if( y1 <= y0 ) {
      if( y1 > (y0 - rgdraw.label_height) )
	yt = y0 - rgdraw.label_height;
      else
	yt = y1;
    } else {
      if( y1 < (y0 + rgdraw.label_height) )
	yt = y0 + rgdraw.label_height;
      else
	yt = y1;
    }
    if( ((x0 >= 0) && (x0 < dispbox.width) &&
	 (y0 >= 0) && (y0 < dispbox.height)) ||
        ((x1 >= 0) && (x1 < dispbox.width) &&
	 (yt >= 0) && (yt < dispbox.height)) ) {
      /* draw the line if it is in the window */
      XDrawLine(dispbox.display, dispbox.ID, gc, x0, y0, x1, y1);
      /* make label indicate function of region */
      len = mark_include(edge_string, region->exclude_region);
      draw_region_label(edge_string, len, gc,
			x1 + 1, yt + rgdraw.label_yoff);
      draw_region_label(center_string, strlen(center_string), gc,
			x0 + 1, y0 + rgdraw.label_yoff);
    }
  }
}

/*
 * Subroutine:	mark_include
 * Purpose:	Add include symbol to string
 */
static int mark_include ( string, exclude )
     char *string;
     int exclude;
{
  int len;
  
  len = strlen(string);
  if( exclude > 0 ) {
    string[len] = '-';
  } else if( exclude == 0 ) {
    string[len] = '+';
  } else
    return( len );
  string[++len] = '\0';
  return( len );
}
