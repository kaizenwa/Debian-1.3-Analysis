#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	csranli.c (Cursor Annuli)
 * Purpose:	Deal with annuli of the software cursor
 * Subroutine:	make_new_annulus()
 * Subroutine:	move_annuli()
 * Subroutine:	update_annuli_centers()			returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  4 June 1989
 *		{n} <who> -- <does what> -- <when>
 *
 *  The interaction for annuli is as follows:
 *  On menu button: set annuli flag (turn button off if point or polygon).
 *  On middle button:
 *   Push: free annuli if grabbed.  Make cursor at mouse or inc position.
 *   Move: size with annuli restrictions
 *   Release: install new annuli record, redraw all cursors.
 *  Left button: move all centers
 */

#include <stdio.h>		/* stderr, NULL, etc. */
#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* cursor colors needed by Cursor.h */
#include "hfiles/cursor.h"	/* define cursor parameter structures */

extern struct colorRec color;	/* need to know color.gcset */

/*
 * Subroutine:	make_new_annulus
 * Purpose:	Copy the cursor onto an annulus
 */
void make_new_annulus ( cursor )
     struct cursorRec *cursor;
{
  struct cursorRec *annulus;
  struct cursorRec *annuli;
  struct cursorRec *copy_cursor();
  int index;

  annulus = copy_cursor (cursor);
  /* annuli are always include regions */
  annulus->exclude_region = 0;
  if( cursor->next_annulus == 0 ) {
    annulus->next_annulus = 0;
    annulus->index = 1;
    cursor->next_annulus = annulus;
  } else {
    if( cursor->next_annulus->win.rayX > annulus->win.rayX ) {
      annulus->next_annulus = cursor->next_annulus;
      annulus->index = 1;
      cursor->next_annulus = annulus;
    } else {
      annuli = cursor->next_annulus;
      /* find sort position for new annulus */
      while( (annuli->next_annulus != 0) &&
	     (annuli->next_annulus->win.rayX < annulus->win.rayX) ) {
	annuli = annuli->next_annulus;
      }
      /* splice in the new annulus */
      annulus->next_annulus = annuli->next_annulus;
      annulus->index = annuli->index + 1;
      annuli->next_annulus = annulus;
    }
    /* updates everybody's index */
    index = annulus->index;
    annuli = annulus->next_annulus;
    while( annuli != NULL ) {
      annuli->index = ++index;
      annuli = annuli->next_annulus;
    }
  }
}

/*
 * Subroutine:	delete_annuli
 * Purpose:	Remove all but the innermost annulus
 * Called by:	select_cursor() in CursorCtrl.c
 */
void delete_annuli ( cursor, collapse )
     struct cursorRec *cursor;
     int collapse;		/* i: inner-ring-becomes-cursor */
{
  struct cursorRec *annulus, *temp;
  void make_cursor(), draw_annuli(), free_cursor();

  if( (annulus = cursor->next_annulus) != NULL ) {
    if( collapse ) {
      /* make cursor like the innermost annulus */
      cursor->win.rayX = annulus->win.rayX;
      cursor->win.rayY = annulus->win.rayY;
      make_cursor (cursor);
    }
    /* if image will not be redrawn, erase the annuli */
    if( !cursor->overwrites_image_data )
      draw_annuli(cursor, &color.gcset.undraw);
    /* delete and free all annuli */
    do {
      temp = annulus->next_annulus;
      free_cursor(annulus);
      annulus = temp;
    } while( annulus != 0 );
    cursor->next_annulus = NULL;
  }
}

/*
 * Subroutine:	move_annuli
 * Purpose:	Change annuli location
 */
void move_annuli ( cursor, winx, winy )
     struct cursorRec *cursor;
     int winx, winy;
{
  int dxm, dym;
  struct cursorRec *annulus;
  void draw_annuli();

  /* HOW MUCH DID WE MOVE? */
  dxm = winx - cursor->win.x;
  dym = winy - cursor->win.y;
  cursor->win.x = winx;
  cursor->win.y = winy;
  cursor->win.X = (double)cursor->win.x + 0.5;
  cursor->win.Y = (double)cursor->win.y + 0.5;
  /* erase the current annuli */
  draw_annuli (cursor, &color.gcset.undraw);
  /* move all the annuli */
  annulus = cursor;
  while( annulus != 0 ) {
    register XPoint *points;
    register int i;
    int point_cnt;

    /* MOVE ALL POINTS */
    point_cnt = annulus->point_cnt;
    points = annulus->points;
    for( i=0; i < point_cnt; i++ ) {
      points[i].x += dxm;
      points[i].y += dym;
    }
    /* UPDATE RECORD OF CENTER */
    annulus->win.x = cursor->win.x;
    annulus->win.y = cursor->win.y;
    annulus->win.X = cursor->win.X;
    annulus->win.Y = cursor->win.Y;
    annulus = annulus->next_annulus;
  }
  /* draw the new annuli */
  draw_annuli (cursor, &color.gcset.track);
}

/*
 * Subroutine:	recenter_annuli_centers
 * Purpose:	Update cursor location params
 */
void update_annuli_centers ( cursor )
     struct cursorRec *cursor;
{
  struct cursorRec *annulus;

  annulus = cursor->next_annulus;
  while( annulus != 0 ) {
    annulus->file.X = cursor->file.X;
    annulus->file.Y = cursor->file.Y;
    annulus = annulus->next_annulus;
  }
}
