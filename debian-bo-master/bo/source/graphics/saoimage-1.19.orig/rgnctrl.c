#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	rgnctrl.c (Region Control)
 * Purpose:	Respond to region commands from various sources
 * Subroutine:	point_region()		returns: void
 * Subroutine:	ascii_region()		returns: void
 * Subroutine:	select_region()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  26 May 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  Define stderr, NULL */
#include <X11/Xlib.h>		/*  X window stuff */
#include <X11/Xutil.h>		/*  X window manager stuff, visuals */
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include "hfiles/constant.h"	/*  Define codes */
#include "hfiles/struct.h"	/*  Declare structure types */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

struct cursorRec *cycle_region = 0;


#ifdef ANSIC

void		point_region(		XButtonEvent *xbutton);
void		ascii_region(		XKeyEvent *xkey, KeySym keysym);
static void	cycle_regions();
static void	reset_regions();

#else

  struct cursorRec *region_indicated_by_pointer();
  int match_region();
  void unsave_region(), set_cursor_file_coords(), disp_dispbox();
  void save_cursor_as_region(), disp_region(), unsave_region();
  void enable_annuli_button(), delete_annuli(), erase_cursor();
  void copy_region_to_cursor(), disp_cursor(), d_transform(), draw_magnifier();
  void label_region_cycle_magnifier(), touch_submenu_button();
  void set_submenu_toggle(), enable_ortho_button();
  void toggle_region_labeling(), toggle_region_visibility();
  void write_regions(), read_regions(), free_cursor();
  static void cycle_regions(), reset_regions();

#endif


/*  Subroutine:	point_region
 *  Purpose:	Respond to a mouse activated request for a point region
 *  Note:	In cursor point mode the mouse buttons map directly:
 *   left - save for include, middle - save for exclude, right - delete
 */
#ifdef ANSIC
void point_region ( XButtonEvent *xbutton )
#else
void point_region ( xbutton )
     XButtonEvent *xbutton;
#endif
{
  struct cursorRec *region;

  /*  Right button is a delete request in point cursor mode  */
  if( xbutton->button == Button3 ) {
    /*  See if mouse indicates a region to delete  */
    region = region_indicated_by_pointer(&cursor, xbutton->x, xbutton->y, 1);
    if( region != NULL ) {
      unsave_region(&cursor, region);
      disp_dispbox();
    }
  } else if( (xbutton->button == Button1) || (xbutton->button == Button2) ) {
    cursor.win.x = xbutton->x;
    cursor.win.y = xbutton->y;
    cursor.win.X = (double)cursor.win.x + 0.5;
    cursor.win.Y = (double)cursor.win.y + 0.5;
    /*  Force file coordinates into agreement with window coordinates  */
    set_cursor_file_coords(&cursor, &coord.disptofile, 1);
    /*  Save cursor as a region  */
    if( xbutton->button == Button1 )
      save_cursor_as_region(&cursor, 0);
    else
      save_cursor_as_region(&cursor, 1);
    cursor.index = cursor.next_region->index;
    disp_region(cursor.next_region);
  }
}


/*  Subroutine:	ascii_region
 *  Purpose:	Keyboard control of regions
 */
#ifdef ANSIC
void ascii_region ( XKeyEvent *xkey, KeySym keysym )
#else
void ascii_region ( xkey, keysym )
     XKeyEvent *xkey;
     KeySym keysym;
#endif
{
  int exclude;
  struct cursorRec *region;

  if( cursor.type == COP_Point ) {
    cursor.win.x = xkey->x;
    cursor.win.y = xkey->y;
    cursor.win.X = (double)cursor.win.x + 0.5;
    cursor.win.Y = (double)cursor.win.y + 0.5;
  }
  /*  Initialize values used or switched in overlapping functions  */
  region = cursor.next_region;
  exclude = 0;
  switch( keysym ) {
  case XK_E:
  case XK_e:
    exclude = 1;
  case XK_S:
  case XK_s:
    /*  Force file coordinates into agreement with window coordinates  */
    set_cursor_file_coords(&cursor, &coord.disptofile, 0);
    set_cursor_file_coords(&cursor, &coord.disptofile, 1);
    save_cursor_as_region(&cursor, exclude);
    disp_region(cursor.next_region);
    break;
  case XK_D:
  case XK_d:
    region = region_indicated_by_pointer(&cursor, xkey->x, xkey->y, 0);
  case XK_Delete:
    if( region != NULL ) {
      unsave_region(&cursor, region);
      disp_dispbox();
    }
    break;
  default:
    break;
  }
}


/*  Subroutine:	select_region
 *  Purpose:	Act on commands from the region button panel
 */
void select_region ( )
{
  switch( control.response[1] ) {
  /*  This case comes up when the main menu button is pressed */
  case 0:
    break;
  case ROP_Cycle:
    cycle_regions();
    break;
  case ROP_Label:
    toggle_region_labeling();
    break;
  case ROP_Omit:
    if( match_region(&cursor, cycle_region) ) {
      unsave_region(&cursor, cycle_region);
      /*  Redisplay without the omitted region  */ 
      disp_dispbox();
    }
    break;
  case ROP_View:
    toggle_region_visibility();
    break;
  case ROP_Reset:
    reset_regions();
    break;
  case ROP_Write:
    write_regions(&cursor, &img, img.file_type);
    break;
  case ROP_Read:
    read_regions();
    break;
  default:
    break;
  }
}


/* Subroutine:	cycle_regions
 * Purpose:	Cylce cursor through saved regions
 */
static void cycle_regions()
{
  float bufx, bufy;
  int annuli_disabled, annuli_on;

  /*  There must be saved regions through which to cycle  */
  if( cursor.next_region == NULL ) {
    (void)fprintf(stderr, "Warning: no saved regions.\n");
    return;
  }
  /*  Determine if annuli button is currently disabled or on  */
  annuli_on = 0;
  if( (cursor.type != COP_Point) && (cursor.type != COP_Polygon) ) {
    annuli_disabled = 0;
    if( cursor.annuli )
      annuli_on = 1;
  } else
    annuli_disabled = 1;
  /*  Erase the cursor while we still know it  */
  if( cursor.overwrites_image_data ) {
    /*  Redraw image and any saved cursors, but not the cursor  */
    if( cursor.next_annulus != NULL )
      delete_annuli (&cursor, 0);
    cursor.annuli = 0;
    cursor.point_cnt = 0;
    cursor.rectangle_cnt = 0;
    disp_dispbox();
  } else {
    if( cursor.next_annulus != NULL )
      /*  Delete_annuli does an erase cursor  */
      delete_annuli(&cursor, 0);
    else
      erase_cursor(&cursor);
  }
  /*  Get next region in cycle  */
  if( cycle_region == NULL )
    cycle_region = cursor.next_region;
  else
    cycle_region = cycle_region->next_region;
  /*  If at end of list, go back to head of list  */
  if( cycle_region == NULL )
    cycle_region = cursor.next_region;
  /*  Make cursor be a copy of this region  */
  copy_region_to_cursor(&cursor, cycle_region);
  /*  If this is a text cursor, set the editor info for this string  */
  if( cursor.type == COP_Text )
    reload_textcursor();
  if( cursor.type != COP_Point )
    /*  Draw the cursor  */
    disp_cursor(&cursor);
  /*  Show exact region position in magnifier  */
  d_transform(&coord.disptobuf, cursor.win.X, cursor.win.Y, &bufx, &bufy);
  draw_magnifier((double)bufx, (double)bufy);
  /*  Label what we just drew  */
  label_region_cycle_magnifier (cycle_region, 1);
  /*  Make sure cursor buttons are correct  */
  touch_submenu_button(COP, cursor.type);
  if( (cursor.type == COP_Box) ||
      (cursor.type == COP_Circle) ||
      (cursor.type == COP_Ellipse) ) {
    if( annuli_disabled )
      enable_annuli_button(1);
    if( annuli_on != cursor.annuli )
      set_submenu_toggle(COP, COP_Annuli, cursor.annuli);
    if( cursor.type == COP_Circle )
      enable_ortho_button(0);
    else
      enable_ortho_button(1);
  } else if( !annuli_disabled ) {
    enable_annuli_button(0);
    enable_ortho_button(0);
  }
}


/*  Subroutine:	reset_regions
 *  Purpose:	Reset all save cursor areas
 */
static void reset_regions()
{
  struct cursorRec *region, *next, *annulus;

  if( (next = cursor.next_region) != NULL ) {
    while( next != NULL ) {
      region = next;
      next = next->next_region;
      /*  Free deleted region and any annuli it might have  */
      do {
	annulus = region->next_annulus;
	free_cursor(region);
	region = annulus;
      } while( region != NULL );
    }
    /*  Reset cycle pointer  */
    cycle_region = NULL;
    cursor.next_region = NULL;
    /*  Redisplay image without the regions  */
    disp_dispbox();
  }
}
