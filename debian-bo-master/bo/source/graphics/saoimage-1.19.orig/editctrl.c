#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/* Module:	editctrl.c (Editor Control)
 * Purpose:	Top level of text editing session for string input
 * Subroutine:	init_edit_popup()		returns: EditStruct *
 * Subroutine:	get_edit_input()		returns: int
 * Subroutine:	unmap_popwin()			returns: void
 * Xlib calls:	XCreateSimpleWindow(), XMapWindow(), XUnmapWindow()
 * Xlib calls:	XSelectInput(), XNextEvent(), XDrawImageString()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		  7 July 1989
 *		{1} Jay Travisano (STScI)  VMS,IMTOOL changes     10 Nov 1989
 *              {2} MVH BSDonly strings.h compatability           19 Feb 1990
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>		/* stderr, NULL, etc. */

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
#include "hfiles/color.h"
#include "hfiles/window.h"
#include "hfiles/edit.h"

extern struct colorRec color;
extern struct windowRec btnbox;
extern struct windowRec desktop;
extern Window root;

#define XOFF 2
#define YOFF 1
#define BDRWDTH 2
#define WDTHLESS 12

struct popRec pop;
static int init_window = 1;

#if VMS && IMTOOL
extern void XZ_ast();
extern int  XZ_efn;
#endif

/*
 * Subroutine:	init_edit_popup
 * Purpose:	Get everything ready for running editor sessions
 */
EditStruct *init_edit_popup ( string, max_chars )
     char *string;
     int max_chars;
{
  EditStruct *edit;
  int len;
  EditStruct *get_edit_struct();
  void init_edit_struct(), load_edit_struct();
  static void init_popwin();

  if( init_window ) {
    init_popwin(color.gcset.menu.foreground, color.gcset.menu.background);
    init_window = 0;
  }
  edit = get_edit_struct (max_chars);
  init_edit_struct(pop.display, pop.ID, edit,
		    pop.fontstruct, pop.foreground, pop.background);
  if( (string != NULL) && ((len = strlen(string)) > 1) )
    load_edit_struct(edit, string, len);
  return( edit );
}

/*
 * Subroutine:	get_edit_input
 * Purpose:	Run popup editor session for text input
 * Xlib calls:	XNextEvent(), XDrawImageString(), XUnmapWindow()
 * Note:	All non-editor events are thrown out during session
 *		(but expose is fielded and configure is saved for end).
 * Returns:	1 if user returns with a response, 0 if user cancels session
 */
int get_edit_input ( edit, one_row, map, unmap, prompt )
     EditStruct *edit;
     int one_row;	/* i: put-edit-after-prompt-on-same-line */
     int map;		/* i: map-window-before-starting */
     int unmap;		/* i: unmap-window-before-return */
     char *prompt;	/* i: user prompt such as "enter file name:" */
{
  XEvent event, configure;
  int init, got_configure, answer;
  GC gc, set_edit_gc();
  int emacs_response();
  void draw_new_string(), redraw_edit_string();
  void adjust_desktop(), redraw_window(), unmap_popwin();
  static void map_popwin();

  map_popwin(edit, one_row, prompt, map);
  if( !map ) {
    /* window already up, don't wait for expose event */
    XClearWindow(pop.display, pop.ID);
    gc = set_edit_gc(pop.font, pop.foreground, pop.background);
    XDrawImageString(pop.display, pop.ID, gc, pop.prompt_x,
		     pop.prompt_y, prompt, strlen(prompt));
    draw_new_string(edit, 0);
    init = 0;
  } else
    init = 1;
  got_configure = 0;
  while( 1 ) {
    /* swallow all the events, except a few, from now until we are done */
    XNextEvent(pop.display, &event);
    switch( event.type ) {
    case KeyPress:
      /* grab key events from all application windows */
      if( (answer = emacs_response(&event.xkey, edit)) != 0 ) {
	if( got_configure )
	  adjust_desktop(&configure.xconfigure);
	if( unmap )
	  unmap_popwin();
	if( answer < 0 )
	  return( 0 );
	else
	  return( 1 );
      }
      break;
    case Expose:
      if( event.xexpose.count > 0 )
	break;
      if( event.xexpose.window == pop.ID ) {
	gc = set_edit_gc(pop.font, pop.foreground, pop.background);
	XDrawImageString(pop.display, pop.ID, gc, pop.prompt_x,
			 pop.prompt_y, prompt, strlen(prompt));
	if( init ) {
	  draw_new_string(edit, 0);
	  init = 0;
	} else
	  redraw_edit_string(edit);
      } else
	/* redraw entire window once (not parts of windows) */
	redraw_window(event.xexpose.window);
      break;
    case ConfigureNotify:
      /* if the main window is reconfigured, save this info */
      if( event.xconfigure.window == desktop.ID ) {
	bcopy((char *)&event, (char *)&configure, sizeof(XEvent));
	got_configure = 1;
      }
      break;
    default:
      break;
    }
  }
}

/*
 * Subroutine:	unmap_popwin
 * Purpose:	Unmap the popwin: called by routines which control map and
 *		unmap themselves because they use the window for multiple
 *		queries (single query can have auto-unmap)
 */
void unmap_popwin ( )
{
  if( pop.pointer_x >= 0 )
    /* if pointer position was noted, return pointer to continue from where
       event processing was interrupted */
    XWarpPointer(pop.display, None, root, 0, 0, 0, 0,
		 pop.pointer_x, pop.pointer_y);
  XUnmapWindow(pop.display, pop.ID);
}

/*
 * Subroutine:	init_popwin
 * Purpose:	Set the popup parameters and create the popup window
 * Xlib calls:	XCreateSimpleWindow(), XSelectInput();
 */
static void init_popwin ( foreground, background )
     unsigned long foreground, background;
{
  int text_yoff;
  XWMHints wmhints;
  XFontStruct *get_fontstruct();

  pop.foreground = foreground;
  pop.background = background;
  /* use the "large" font */
  pop.fontstruct = get_fontstruct (2);
  pop.font = pop.fontstruct->fid;
  pop.font_height = pop.fontstruct->ascent + pop.fontstruct->descent;
  pop.ref_width = btnbox.width;
  pop.ref_height = btnbox.height;
  pop.width = pop.ref_width - (XOFF + XOFF + BDRWDTH + BDRWDTH);
  pop.height = pop.ref_height - (YOFF + YOFF + BDRWDTH + BDRWDTH);
  text_yoff = (pop.height - (pop.font_height + pop.font_height)) / 3;
  pop.two_y1 = pop.font_height + text_yoff - pop.fontstruct->descent;
  pop.two_y2 = pop.two_y1 + pop.font_height + text_yoff;
  pop.one_y = (pop.height + pop.fontstruct->ascent) / 2;
  pop.prompt_x = pop.two_y1;
  pop.display = btnbox.display;
  pop.ID =
    XCreateSimpleWindow(btnbox.display, btnbox.ID, XOFF, YOFF,
			(unsigned int)pop.width, (unsigned int)pop.height,
			BDRWDTH, pop.foreground, pop.background);
#if VMS && IMTOOL
  XSelectAsyncInput(pop.display, pop.ID, ExposureMask | KeyPressMask,
	  	XZ_ast, XZ_efn);
#endif
  XSelectInput(pop.display, pop.ID, ExposureMask | KeyPressMask);
  /* Tell window manager to handle focus for us */
  wmhints.input = True;
  wmhints.flags = InputHint;
  XSetWMHints(btnbox.display, pop.ID, &wmhints);
}

/*
 * Subroutine:	map_popwin
 * Purpose:	map the popwindow, noting its current size and setting params
 * Xlib calls:	XMapWindow()
 */
static void map_popwin ( edit, one_row, prompt, map )
     EditStruct *edit;
     int one_row;	/* i: put-edit-after-prompt-on-same-line */
     char *prompt;	/* i: prompt string such as "Enter file name:" */
     int map;		/* i: map-the-window-when-ready */
{
  int text_yoff;
  Window root_ret, child_ret;
  int x_ret, y_ret;
  unsigned int mask_ret;

  if( (pop.ref_width != btnbox.width) || (pop.ref_height != btnbox.height) ) {
    pop.ref_width = btnbox.width;
    pop.ref_height = btnbox.height;
    pop.width = pop.ref_width - (XOFF + XOFF + BDRWDTH + BDRWDTH);
    pop.height = pop.ref_height - (YOFF + YOFF + BDRWDTH + BDRWDTH);
    XResizeWindow(btnbox.display, pop.ID,
		  (unsigned int)pop.width, (unsigned int)pop.height);
    text_yoff = (pop.height - (pop.font_height + pop.font_height)) / 3;
    pop.two_y1 = pop.font_height + text_yoff;
    pop.two_y2 = pop.two_y1 + pop.font_height + text_yoff;
    pop.one_y = (pop.height + pop.fontstruct->ascent) / 2;
    pop.prompt_x = pop.two_y1;
  }
  /* we don't know how current this edit's coords are, always update them */
  if( one_row ) {
    pop.prompt_y = pop.one_y;
    edit->y = pop.one_y;
    edit->x =
      pop.prompt_x + XTextWidth(pop.fontstruct, prompt, strlen(prompt)) + 10;
  } else {
    pop.prompt_y = pop.two_y1;
    edit->y = pop.two_y2;
    edit->x = pop.prompt_x;
  }
  edit->max_pixlen = pop.width - (edit->x + pop.prompt_x);
  edit->area_y = edit->y - edit->fontstruct->max_bounds.ascent;
  if( map ) {
    if( XQueryPointer(pop.display, root, &root_ret, &child_ret, &x_ret, &y_ret,
		      &pop.pointer_x, &pop.pointer_y, &mask_ret) == False )
      pop.pointer_x = -1;
    XMapWindow (btnbox.display, pop.ID);
  }
}
                                                          
                                                               
                                                               
                                                              

                                                               
                                                               
 
