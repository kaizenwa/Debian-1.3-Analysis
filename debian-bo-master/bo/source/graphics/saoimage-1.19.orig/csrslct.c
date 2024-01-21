#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrslct.c (Cursor Select)
 * Purpose:	Initialize and change states of the software cursor
 * Subroutine:	select_cursor()			returns: void
 * Subroutine:	init_software_cursors()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 4 June 1989
 *		{1} MVH added arrow and text cursors	          1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/*  stderr, NULL, etc.  */
#include <X11/Xlib.h>		/*  X window stuff  */
#include <X11/Xutil.h>		/*  X window manager stuff  */
#include "hfiles/constant.h"	/*  Define codes  */
#include "hfiles/struct.h"	/*  Declare structure types  */
#include "hfiles/extern.h"	/*  Extern main parameter structures  */

#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		select_cursor();
void		init_software_cursors();

#else

  int init_vertices();
  void make_cursor(), disp_dispbox(), erase_cursor(), enable_ortho_button();
  void set_submenu_toggle(), delete_annuli(), enable_annuli_button();
  void collapse_polygon(), start_polygon(), make_new_annulus(), disp_cursor();
  void clear_textcursor(), new_textcursor();
  char *calloc_errchk();
  void note_current_disp_transform(), d_transform();

#endif


/*  Subroutine:	select_cursor
 *  Purpose:	Respond to command input from user interface
 */
void select_cursor ()
{
  /*  If main menu selection, set mode(s) and return  */
  if( control.response[1] == 0 ) {
    control.mode = COP;
    if( cursor.type == COP_Text )
      grab_keys_for_textcursor(1);
    return;
  }
  if( (control.response[1] & COP_Shape) != 0 ) {
    /*  Erase the current cursor and set the new one  */
    if( cursor.annuli ) {
      /*  Annuli cannot be an initial condition (erases if in overlay)  */
      delete_annuli(&cursor, 1);
      cursor.annuli = 0;
      set_submenu_toggle(COP, COP_Annuli, 0);
    }
    if( cursor.overwrites_image_data ) {
      int point_cnt = cursor.point_cnt;
      cursor.point_cnt = 0;
      /*  Polygon is either collapsed or eliminated, so no need to save cnt  */
      cursor.rectangle_cnt = 0;
      /*  Redraw image and any saved cursors  */
      disp_dispbox();
      cursor.point_cnt = point_cnt;
    } else {
      /*  Just erase the cursor  */
      erase_cursor(&cursor);
    }
    /*  If changing cursor type  */
    if( cursor.type != control.response[1] ) {
      if( cursor.type == COP_Polygon ) {
	/*  Suppress hash-marks and vertex list  */
	cursor.rectangle_cnt = 0;
	cursor.poly_cnt = 0;
      } else if( cursor.type == COP_Text ) {
	clear_textcursor();
      }
      cursor.type = control.response[1];
      /*  Set vertex coordinates and any other environment stuff  */
      if( (cursor.type == COP_Box) ||
	  (cursor.type == COP_Ellipse) ) {
	enable_annuli_button(1);
	enable_ortho_button(1);
	make_cursor(&cursor);
      } else if( cursor.type == COP_Circle ) {
	enable_annuli_button(1);
	enable_ortho_button(0);
	make_cursor(&cursor);
      } else {
	enable_annuli_button(0);
	enable_ortho_button(0);
	if( cursor.type == COP_Polygon ) {
	  /*  Prepare special polygon facilities  */
	  start_polygon(&cursor, &coord);
	} else if( cursor.type == COP_Text ) {
	  new_textcursor();
	} else {
	  /*  This leaves Point and Arrow  */
	  make_cursor(&cursor);
	}
      }
    } else if( cursor.type == COP_Polygon ) {
      /*  Reclicking on polygon resets all its points  */
      collapse_polygon(&cursor);
    }
  } else if( control.response[1] == COP_Annuli ) {
    if( cursor.annuli ) {
      /*  If turning off annuli  */
      cursor.annuli = 0;
      if( cursor.next_annulus != 0 ) {
	delete_annuli(&cursor, 1);
	if( cursor.overwrites_image_data )
	  /*  Redraw image and cursor  */
	  disp_dispbox();
      }
      if( cursor.type != COP_Circle )
	enable_ortho_button(1);
    } else {
      /*  If turning on annuli  */
      if( (cursor.type == COP_Point) || (cursor.type == COP_Polygon) ) {
	/*  This should never happen  */
	set_submenu_toggle(COP, COP_Annuli, 0);
      } else {
	cursor.annuli = 1;
	/*  Current cursor becomes first annulus  */
	make_new_annulus(&cursor);
      }
      enable_ortho_button(0);
    }
  } else if( control.response[1] == COP_Orthogonal ) {
    if( cursor.annuli )
      return;
    if( cursor.overwrites_image_data ) {
      int point_cnt = cursor.point_cnt;
      cursor.point_cnt = 0;
      /*  Polygon is either collapsed or eliminated, so no need to save cnt  */
      cursor.rectangle_cnt = 0;
      /*  Redraw image and any saved cursors  */
      disp_dispbox();
      cursor.point_cnt = point_cnt;
    } else {
      /*  Just erase the cursor  */
      erase_cursor(&cursor);
    }
    cursor.rot.angle = 0.0;
    cursor.rot.cos = -1.0;
    cursor.rot.sin = 0.0;
    if( (cursor.type == COP_Ellipse) || (cursor.type == COP_Box) )
      make_cursor(&cursor);
    else
      return;
  }
  /*  Draw the cursor  */
  disp_cursor(&cursor);
  /*  If cursor isn't the mode, make it the mode  */
  control.mode = COP;
}


/*  Subroutine:	init_software_cursors
 *  Purpose:	Initialize the cursor parameters
 *  Prestate:	Color must be determined and dispbox must be created and sized
 */
void init_software_cursors ()
{
  cursor.points =
    (XPoint *)calloc_errchk(CURSOR_MAX + 1, sizeof(XPoint), "Cursor");
  /*  Record the current display parameters, to later recognize a change  */
  note_current_disp_transform(&coord.filetodisp);
  /*  If cursor position not set, center it on image  */
  if( (cursor.file.X < 0.0) || (cursor.file.Y < 0.0) )
    d_transform(&coord.imgtofile, (double)coord.img.cenX,
		(double)coord.img.cenY, &cursor.file.X, &cursor.file.Y);
  if( (cursor.type == COP_Polygon) || (cursor.type == COP_Point) ) {
    cursor.annuli = 0;
    if( cursor.type == COP_Polygon )
      /*  Prepare special polygon facilities  */
      start_polygon(&cursor, &coord);
  } else {
    make_cursor (&cursor);
    if( cursor.annuli )
      /*  Current cursor becomes first annulus  */
      make_new_annulus(&cursor);
  }
  /*  Set cursor drawing color params  */
  cursor.win.display = dispbox.display;
  cursor.win.ID = dispbox.ID;
  cursor.overwrites_image_data = !color.cursor_overlay;
  cursor.draw = &color.gcset.draw;
}
