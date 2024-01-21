#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	mgfymark.c (Magnify Mark)
 * Purpose:	Quickly put the sighting mark in the magnified image
 * Subroutine:	init_magnifier_Zmark()		returns: void
 * Subroutine:	mark_magnifierZ()		returns: void
 * Subroutine:	init_magnifier_XYmark()		returns: void
 * Subroutine:	mark_magnifierXY()		returns: void
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		 6 June 1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <X11/Xlib.h>		/* X window stuff */
#include <X11/Xutil.h>		/* X window manager stuff */
#include "hfiles/color.h"	/* define GCspec */
#include "hfiles/magnify.h"	/* magnifier quick access structure */
#include "defs/magnify.def"	/* masks for sighting mark */

extern struct magRec magset;

struct {
  char *top_left;
  char *top_right;
  char *bot_left;
  char *bot_right;
  int tl_br_next;
  int tr_bl_next;
} Zmark;

/*
 * Subroutine:	init_magnifier_Zmark
 */
void init_magnifier_Zmark ( )
{
  char *line_center;

  line_center = magset.data + magset.data_x_hot +
    ((magset.data_y_hot - Zmark_y_hot) * magset.win.width);
  Zmark.top_left = line_center - Zmark_x_hot;
  Zmark.top_right = line_center + Zmark_x_hot;
  line_center = magset.data + magset.data_x_hot +
    ((magset.data_y_hot + Zmark_y_hot) * magset.win.width);
  Zmark.bot_left = line_center - Zmark_x_hot;
  Zmark.bot_right = line_center + Zmark_x_hot;
  Zmark.tl_br_next = magset.win.width - Zmark_x_hot;
  Zmark.tr_bl_next = magset.win.width + Zmark_x_hot;
}

/*
 * Subroutine:	mark_Zmagnifier
 */
void mark_Zmagnifier ( )
{
  register char *top_left, *top_right;
  register char *bot_left, *bot_right;
  register int *mark;
  register int foreground, background;

  top_left = Zmark.top_left;
  bot_left = Zmark.bot_left;
  top_right = Zmark.top_right;
  bot_right = Zmark.bot_right;
  foreground = magset.gcset_aim->foreground;
  background = magset.gcset_aim->background;
  mark = (int *)Zmark_mask;
  while( top_left < bot_left ) {
    while( top_left < top_right ) {
      if( *mark ) {
	if( *mark > 1 ) {
	  *top_left = foreground;
	  *top_right = foreground;
	  *bot_left = foreground;
	  *bot_right = foreground;
	} else {
	  *top_left = background;
	  *top_right = background;
	  *bot_left = background;
	  *bot_right = background;
	}
      }
      mark++;
      top_left++;
      top_right--;
      bot_left++;
      bot_right--;
    }
    /* pointers converge at center, just do left */
    if( *mark ) {
      if( *mark > 1 ) {
	*top_left = foreground;
	*bot_left = foreground;
      } else {
	*top_left = background;
	*bot_left = background;
      }
    }
    mark++;
    top_left += Zmark.tl_br_next;
    bot_right -= Zmark.tl_br_next;
    top_right += Zmark.tr_bl_next;
    bot_left -= Zmark.tr_bl_next;
  }
  /* pointers converge on middle line, just do top */
  while( top_left < top_right ) {
    if( *mark ) {
      if( *mark > 1 ) {
	*top_left = foreground;
	*top_right = foreground;
      } else {
	*top_left = background;
	*top_right = background;
      }
    }
    mark++;
    top_left++;
    top_right--;
  }
  /* pointers converge at center, just do left */
  if( *mark ) {
    if( *mark > 1 ) {
      *top_left = foreground;
    } else {
      *top_left = background;
    }
  }
}

struct {
  char *top_left;
  unsigned char *mark;
  unsigned char *mask;
  unsigned char *last_mark;
  int bytes;
  int next_line;
} XYmark;

/*
 * Subroutine:	init_magnifier_XYmark
 */
void init_magnifier_XYmark ( )
{
  int xoff;

  xoff = magset.data_x_hot - XYmark_x_hot;
  XYmark.top_left = magset.data + (xoff / 8) +
    ((magset.data_y_hot - XYmark_y_hot) * magset.bytes_per_line);
  xoff = xoff & 7; 
  XYmark.mark = XYmark_mark[xoff] - 1;
  XYmark.mask = XYmark_mask[xoff] - 1;
  if( xoff < 4 ) {
    XYmark.bytes = 2;
    XYmark.next_line = magset.bytes_per_line - 1;
    XYmark.last_mark = XYmark.mark + 26;
  } else {
    XYmark.bytes = 3;
    XYmark.next_line = magset.bytes_per_line - 2;
    XYmark.last_mark = XYmark.mark + 39;
  }
}

/*
 * Subroutine:	mark_XYmagnifier
 */
void mark_XYmagnifier ( )
{
  register unsigned char *data, *mark, *mask, *last_mark;
  register int next_line;

  data = (unsigned char *)XYmark.top_left;
  mark = (unsigned char *)XYmark.mark;
  mask = (unsigned char *)XYmark.mask;
  last_mark = (unsigned char *)XYmark.last_mark;
  next_line = XYmark.next_line;

  if( XYmark.bytes == 2 ) {
    while( mark < last_mark ) {
      *data = (*data & *(++mask)) | *(++mark);
      data++;
      *data = (*data & *(++mask)) | *(++mark);
      data += next_line;
    }
  } else {
    while( mark < last_mark ) {
      *data = (*data & *(++mask)) | *(++mark);
      data++;
      *data = ((*data & *(++mask)) | *(++mark));
      data++;
      *data = (*data & *(++mask)) | *(++mark);
      data += next_line;
    }
  }
}
