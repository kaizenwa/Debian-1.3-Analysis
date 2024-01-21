#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csrcoord.c (Cursor Coordinates)
 * Purpose:	Calculate coordinates for cursor functions
 * Subroutine:	set_cursor_file_coords()	returns: void	
 * Subroutine:	adjust_cursor_coords()		returns: void	
 * Subroutine:	set_cursor_from_file_coords()	returns: void	
 * Subroutine:	set_annuli_from_file_coords()	returns: void
 * Subroutine:	note_current_disp_transform()	returns: void	
 * Subroutine:	report_cursor_info()		returns: void	
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{1} MVH added text cursor support		   1 Jan 1991
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/constant.h"	/* define codes */
#include "hfiles/coord.h"	/* coord structs */
#include "hfiles/cursor.h"	/* define cursor parameter structures */

/*  Saved transform to see if it changed  */
static Transform ftod;


#ifdef ANSIC
/*  Exported declarations must be centralized before ANSI C can be used  */

void		set_cursor_file_coords(	struct cursorRec *cursor,
					Transform *disptofile, int setcen);
void		adjust_cursor_coords(	struct cursorRec *cursor,
					struct coordRec *coord);
void		set_cursor_from_file_coords(	struct cursorRec *cursor,
						Transform *filetodisp);
void		set_annuli_from_file_coords(	struct cursorRec *cursor,
						Transform *filetodisp);
void		note_current_disp_transform(	Transform *filetodisp);
void		report_cursor_info(	struct cursorRec *cursor);

static int	disp_coords_changed(	Transform *filetodisp);

#else

void d_transform(), make_cursor(), reset_textcursor_coords();
double cursor_area();
void set_polygon_from_file_coords(), set_annuli_from_file_coords();
void set_cursor_from_file_coords(), note_current_disp_transform();
static int disp_coords_changed();

#endif


/*  Subroutine:	set_cursor_file_coords
 *  Purpose:	Set img coords of cursor
 */
#ifdef ANSIC
void set_cursor_file_coords( struct cursorRec *cursor, Transform *disptofile,
			     int setcen )
#else
void set_cursor_file_coords ( cursor, disptofile, setcen )
     struct cursorRec *cursor;
     Transform *disptofile;
     int setcen;	/* i: set-center-after-move, else set dim after size */
#endif
{
  double ratio;

  if( setcen ) {
    d_transform(disptofile, cursor->win.X, cursor->win.Y,
		&cursor->file.X, &cursor->file.Y);
  } else {
    if( (ratio = disptofile->inx_outx + disptofile->inx_outy) < 0.0 )
      ratio = -ratio;
    cursor->file.Xdim = cursor->win.rayX * ratio;
    cursor->file.Ydim = cursor->win.rayY * ratio;
  }
}


/*  Subroutine:	adjust_cursor_coords
 *  Purpose:	If the display image has been altered, change the cursor
 *		parameters in the same way, so that the cursor has the same
 *		relationship to the image.  old cursor is not erased, image
 *		redraw usually does that
 */
#ifdef ANSIC
void adjust_cursor_coords ( struct cursorRec *cursor, struct coordRec *coord )
#else
void adjust_cursor_coords ( cursor, coord )
     struct cursorRec *cursor;
     struct coordRec *coord;
#endif
{
  int maincursor = 1;

  if( disp_coords_changed(&coord->filetodisp) ) {
    /*  Remake the window coordinates and line drawing records  */
    while( cursor != NULL ) {
      if( cursor->type == COP_Polygon )
	set_polygon_from_file_coords(cursor, &coord->filetodisp, maincursor);
      else {
	if( cursor->annuli )
	  set_annuli_from_file_coords(cursor, &coord->filetodisp);
	else
	  set_cursor_from_file_coords(cursor, &coord->filetodisp);
      }
      /*  Loop on saved cursors  */
      maincursor = 0;
      cursor = cursor->next_region;
    }
    note_current_disp_transform(&coord->filetodisp);
  }
}


/*  Subroutine:	set_cursor_from_file_coords
 *  Purpose:	Set cursor window coordinates from its file coordinates
 */
#ifdef ANSIC
void set_cursor_from_file_coords( struct cursorRec *cursor,
				  Transform *filetodisp )
#else
void set_cursor_from_file_coords ( cursor, filetodisp )
     struct cursorRec *cursor;
     Transform *filetodisp;
#endif
{
  float X, Y, ratio;

  /*  Get new window coordinates  */
  d_transform(filetodisp,
	      (double)cursor->file.X, (double)cursor->file.Y, &X, &Y);
  cursor->win.X = X;
  cursor->win.Y = Y;
  cursor->win.x = (int)X;
  cursor->win.y = (int)Y;
  /*  Get new window dimensions  */
  if( (ratio = filetodisp->inx_outx + filetodisp->inx_outy) < 0.0 )
    ratio = -ratio;
  cursor->win.rayX = ratio * cursor->file.Xdim;
  cursor->win.rayY = ratio * cursor->file.Ydim;
  /*  Make new drawing vertices  */
  if( cursor->type == COP_Text )
    reset_textcursor_coords(cursor);
  else
    make_cursor(cursor);
}


/*  Subroutine:	set_annuli_from_file_coords
 *  Purpose:	Set annuli window coordinates from their file coordinates
 */
#ifdef ANSIC
void set_annuli_from_file_coords ( struct cursorRec *cursor,
				   Transform *filetodisp )
#else
void set_annuli_from_file_coords ( cursor, filetodisp )
     struct cursorRec *cursor;
     Transform *filetodisp;
#endif
{
  struct cursorRec *annulus;
  float X, Y, ratio;

  /*  Get new window coordinates  */
  d_transform(filetodisp,
	      (double)cursor->file.X, (double)cursor->file.Y, &X, &Y);
  cursor->win.X = X;
  cursor->win.Y = Y;
  cursor->win.x = (int)X;
  cursor->win.y = (int)Y;
  /*  Get new window dimensions  */
  if( (ratio = filetodisp->inx_outx + filetodisp->inx_outy) < 0.0 )
    ratio = -ratio;
  annulus = cursor->next_annulus;
  while( annulus != 0 ) {
    annulus->win.X = cursor->win.X;
    annulus->win.Y = cursor->win.Y;
    annulus->win.x = cursor->win.x;
    annulus->win.y = cursor->win.y;
    annulus->win.rayX = ratio * annulus->file.Xdim;
    annulus->win.rayY = ratio * annulus->file.Ydim;
    /*  Make new drawing vertices  */
    make_cursor(annulus);
    annulus = annulus->next_annulus;
  }
}


/*  Subroutine:	note_current_disp_transform
 *  Purpose:	Note current transform
 */
#ifdef ANSIC
void note_current_disp_transform ( Transform *filetodisp )
#else
void note_current_disp_transform ( filetodisp )
     Transform *filetodisp;
#endif
{
  ftod.inx_outx = filetodisp->inx_outx;
  ftod.inx_outy = filetodisp->inx_outy;
  ftod.add_outx = filetodisp->add_outx;
  ftod.add_outy = filetodisp->add_outy;
}


/*  Subroutine:	disp_params_changed
 *  Purpose:	Check if current transform has changed
 */
#ifdef ANSIC
static int disp_coords_changed ( Transform *filetodisp )
#else
static int disp_coords_changed ( filetodisp )
     Transform *filetodisp;
#endif
{
  if( (filetodisp->inx_outx != ftod.inx_outx) ||
      (filetodisp->inx_outy != ftod.inx_outy) ||
      (filetodisp->add_outx != ftod.add_outx) ||
      (filetodisp->add_outy != ftod.add_outy) )
    return( 1 );
  else
    return( 0 );
}


/*  Subroutine:	report_cursor_info
 *  Purpose:	Calculate and report curosr params in file coordinates
 */
#ifdef ANSIC
void report_cursor_info ( struct cursorRec *cursor )
#endif
void report_cursor_info ( cursor )
     struct cursorRec *cursor;
{
  double area;
  int i;

  /*  Calculate the new coords  */
  /*  Now tell what and where  */
  (void)printf("cursor window position: x=%d, y=%d\n",
	       cursor->win.x, cursor->win.y);
  (void)printf("cursor file position: X=%.3f, Y=%.3f\n",
	       cursor->file.X, cursor->file.Y);
  switch( cursor->type ) {
  case COP_Circle:
    (void)printf("Circle cursor\n");
    (void)printf("radius: %.2f, ", cursor->file.Xdim);
    (void)printf("geometric area: %.2f\n", cursor_area(cursor, 1));
    break;
  case COP_Ellipse:
    (void)printf("Rotatable ellipse\n");
    (void)printf("angle in radians: %6.3f\n", cursor->rot.angle);
    (void)printf("X radius: w=%.2f, Y radius: h=%.2f\n",
		 cursor->file.Xdim, cursor->file.Ydim);
    (void)printf("geometric area: %.2f\n", cursor_area(cursor, 1));
    break;
  case COP_Box:
    if( cursor->rot.angle == 0.0 ) {
      (void)printf("Orthogonal box cursor\n");
      (void)printf("width and height in file pixels: w=%.2f, h=%.2f\n",
		   cursor->file.Xdim * 2.0, cursor->file.Ydim * 2.0);
      (void)printf("geometric area: %.2f\n", cursor_area(cursor, 1));
    } else {
      (void)printf("Rotated box cursor\n");
      (void)printf("angle in radians: %6.3f\n", cursor->rot.angle);
      (void)printf("width and height in file pixels: w=%.2f, h=%.2f\n",
		   cursor->file.Xdim * 2.0, cursor->file.Ydim * 2.0);
      (void)printf("geometric area: %.2f\n", cursor_area(cursor, 1));
    }
    break;
  case COP_Polygon:
    (void)printf("Polygon cursor with ");
    (void)printf("%d vertices:\n", cursor->poly_cnt);
    for( i=0; i<cursor->poly_cnt; i++ ) {
      (void)printf("  (x=%.2f, y=%.2f)\n",
		   cursor->poly[i].fileX, cursor->poly[i].fileY);
    }
    if( (area = cursor_area(cursor, 1)) > 0.0 )
      (void)printf("geometric area: %.2f\n", area );
    else
      (void)printf("Note: polygon is twisted.\n");
    break;
  case COP_Point:
  default:
    break;
  }
  (void)printf("\n");	
}
