#ifndef lint
static char SccsId[] = "%W%  %G%";
#endif

/*
 * Module:	create.c (Make Box)
 * Project:	PROS -- ROSAT RSDC
 * Purpose:	Create a complete (unconnected) buttonbox in the given window
 * Subroutines:	MakeButtonBox()			returns: ButtonBox
 * Subroutines:	static btn_MakeWindows()	returns: void
 * Subroutines:	btn_LabelButtons()		returns: void
 * Subroutines:	SetTAEButtonLook()		returns: void
 * Xlib calls:	XMakeSimpleWindow(), XSelectInput(), XMapSubwindows()
 * Copyright:	1989 Smithsonian Astrophysical Observatory
 *		You may do anything you like with this file except remove
 *		this copyright.  The Smithsonian Astrophysical Observatory
 *		makes no representations about the suitability of this
 *		software for any purpose.  It is provided "as is" without
 *		express or implied warranty.
 * Modified:	{0} Michael VanHilst	initial version		21 March 1989
 *		{1} Jay Travisano (STScI)  VMS,IMTOOL changes   10 Nov   1989
 *		{n} <who> -- <does what> -- <when>
 */

#include <stdio.h>	/* define stderr */
#include <X11/Xlib.h>	/* define Xlib types and calls */
#include "buttons.h"

/* 16x16 (2 bytes * 16) bitmaps with default corner patterns of border */
#include "borders.h"		/* static BorderPatterns defBorders */
#include "motf.h"

static int motf_look = 0;	/* if set, buttons harmonize better w/ motif */

#if VMS && IMTOOL
extern void XZ_ast();
extern int  XZ_efn;
#endif

static GC def_gc = NULL;
/*
 * Subroutine:	MakeButtonBox
 * Purpose:	Do everything necessary to make a new buttonbox
 * Returns:	ButtonBox handle for the new buttonbox
 * Called by:	Application program
 * Uses:	btn_Alloc() in BtnAlloc.c
 * Uses:	btn_CreateWindows(), btn_LabelButtons() below
 * Xlib calls:	XCreateSimpleWindow(), XMapSubwindows()
 * Xlib calls:	DefaultScreen, DefaultGC, DefaultVisual, DefaultDepth
 * Post-state:	No buttons are in on state and no submenus are attached
 * Method:	Make a subwindow exactly covering parent window.  Fill it with
 *		subwindows as buttons, create drawing stuff for each button
 * Note:	Buttonboxes must later be combined into menus and panels
 * Note:	Call TouchButton to activate buttons and submenus when ready
 */
ButtonBox MakeButtonBox ( parent, gc, visual, background, geo, defs, borders )
     BoxParent *parent;		/* i: Parent window info */
     GC gc;			/* i: graphics context for drawing labels */
     Visual *visual;		/* i: visual for XCreateImage() */
     unsigned long background;	/* i: window background pixel value */
     BoxGeometry *geo;		/* i: button arrangement and count info */
     ButtonSpec defs[];		/* i: Button descriptors */
     BorderPatterns *borders;	/* i: optional non-default border look */
{
  ButtonBox buttonbox;		/* o: handle for buttonbox record */
  unsigned int width, height;	/* l: dimensions of parent window */
  Display *display;		/* l: X server */
  int screen;			/* l: screen number for default stuff */
  int btn_wdth, btn_hght;	/* l: dimensions of all buttons */
  int x, y;			/* l: coordinates of box in parent */
  int i;			/* l: loop counter */
  char *btn_Alloc();
  static void btn_CreateWindows();
  void btn_LabelButtons(), btn_Dimensions();

  /* allocate the space for the new record */
  buttonbox = (ButtonBox)
    btn_Alloc(1, sizeof(struct _ButtonBox), "Buttonbox");
  /* put server connection display and screen details in the record */
  display = parent->display;
  buttonbox->display = display;
  buttonbox->parentID = parent->wndwID;
  screen = DefaultScreen(display);
  if( gc == NULL ) {
    /* use default buttonlib gc (create if not yet in existence */
    if( def_gc == NULL ) {
      def_gc = XCreateGC(display, parent->wndwID, 0, NULL);
      XSetState(display, def_gc, 1, 0, GXcopy, 1);
    }
    gc = def_gc;
  }
  if( visual != (Visual *)0 )
    buttonbox->visual = visual;
  else
    buttonbox->visual = DefaultVisual(display, screen);
  /* get the basic box and button dimensions */
  btn_Dimensions(parent, geo, &btn_wdth, &btn_hght, &x, &y, &width, &height);
  /* create a buttonbox subwindow (it gets no events) */
  buttonbox->wndwID =
    XCreateSimpleWindow(display, parent->wndwID, x, y, width, height,
			(unsigned int)0, (unsigned long)0, background);
  buttonbox->parent_width = parent->width;
  buttonbox->parent_height = parent->height;
  i = geo->box_rows * geo->box_cols;
  /* check counts */
  if( geo->btn_cnt > i ) {
    (void)fprintf(stderr, "WARNING: More labels than buttons: %s %d\n",
		  defs[0].feel->title, geo->btn_cnt);
  geo->btn_cnt = i;
  }
  buttonbox->geometry = geo;
  buttonbox->btn_cnt = geo->btn_cnt;
  /* create space for the button records */
  buttonbox->buttons = (ButtonRecord *)
       btn_Alloc(buttonbox->btn_cnt, sizeof(ButtonRecord), "Buttons");
  /* create space for a list of window id's (last one is this window) */
  buttonbox->window_list = (Window *)
       btn_Alloc(buttonbox->btn_cnt + 1, sizeof(Window *), "Button list");
  buttonbox->window_list[buttonbox->btn_cnt] = buttonbox->wndwID;
  buttonbox->window_count = buttonbox->btn_cnt + 1;
  /* set pointers for user specified (and default) look and feel */
  if( borders != NULL )
    buttonbox->borders = borders;
  else {
    if( motf_look )
      buttonbox->borders = &motfBorders;
    else
      buttonbox->borders = &defBorders;
  }
  for( i = 0; i < buttonbox->btn_cnt; i++ ) {
    buttonbox->buttons[i].look = defs[i].look;
    buttonbox->buttons[i].feel = defs[i].feel;
  }
  /* initially no mouse buttons are down */
  buttonbox->down_btn = -1;
  buttonbox->down_mouse_btn = -1;
  buttonbox->mode_btn = -1;
  buttonbox->co_mode_btn = -1;
  /* create button subwindows and fill associated records */
  btn_CreateWindows (buttonbox, gc, btn_wdth, btn_hght);
  /* create XImage bitmaps for button visuals */
  btn_LabelButtons(buttonbox, btn_wdth, btn_hght,
		   geo->off_inverse, geo->on_inverse);
  /* make the buttons mapped (but not yet the box itself) */
  XMapSubwindows(display, buttonbox->wndwID);
  return( buttonbox );
}

/*
 * Subroutine:	btn_Dimensions
 * Purpose:	Set six dimensions used to create box and its buttons
 * Returns:	void
 * Called by:	MakeButtonbox() above, ResizeBox() in ResizeBox.c
 * Xlib calls:	none
 * Post-state:	6 integer params are set
 * Method:	Dimensions of buttons are to fill window area with full
 *		arrangement.  Adjustment is made to center within excess, if
 *		any.  X and Y position the box'x subset arrangement and
 *		width and height are the subset's dimensions.
 */
void btn_Dimensions ( parent, geo, btn_wdth, btn_hght, x, y, width, height )
     BoxParent *parent;			/* i: Parent window info */
     BoxGeometry *geo;			/* i: arrangement and count info */
     int *btn_wdth, *btn_hght;		/* o: dimensions of each buttons */
     int *x, *y;			/* o: coordinates of box in parent */
     unsigned int *width, *height;	/* o: dimensions of box subwindow */
{
  int xx, yy;
  *btn_wdth = (int)((double)(parent->xwdth) / geo->parent_cols);
  *btn_hght = (int)((double)(parent->yhght) / geo->parent_rows);
  *width = geo->box_cols * *btn_wdth;
  *height = geo->box_rows * *btn_hght;
  /* offset centers buttons when fit is not perfect */
  /* parent x + box x + 1/2 box's slop */
  xx = parent->x + (int)(geo->box_col_x * (double)(parent->xwdth)) +
    (((int)((double)(parent->xwdth) *
	    ((double)(geo->box_cols) / geo->parent_cols)) + 1 -
      *width) / 2);
  yy = parent->y + (int)(geo->box_row_y * (double)(parent->yhght)) +
    (((int)((double)(parent->yhght) *
	  ((double)(geo->box_rows) / geo->parent_rows)) -
      *height) / 2);
  if( xx < 0 )
    *x = 0;
  else
    *x = xx;
  if( yy < 0 )
    *y = 0;
  else
    *y = yy;
}

/*
 * Subroutine:	btn_CreateWindows
 * Purpose:	Create a subwindow for each button and set its events
 * Returns:	void
 * Called by:	MakeButtonBox() above
 * Xlib calls:	XCreateSimpleWindow(), XSelectInput()
 * Pre-state:	surrounding window created for perfect fit
 * Post-state:	Buttonbox has window and display filled for all buttons,
 *		and window_list is filled.
 * Method:	All buttons are the same size, and arranged according to
 *		ButtonBox rows and cols specifications.
 * Note:	Each button is a borderless window.
 */
static void btn_CreateWindows( buttonbox, gc, btn_wdth, btn_hght )
     ButtonBox buttonbox;	/* i: handle of buttonbox record */
     GC gc;			/* i: graphics context for drawing labels */
     int btn_wdth, btn_hght;	/* i: dimensions of each buttons */
{
  int x, y;
  int j, i;
  int btn;
  unsigned long background;
  Display *display;
  Window wndwID;
  BoxGeometry *geo;

  btn = 0;
  display = buttonbox->display;
  wndwID = buttonbox->wndwID;
  background = buttonbox->background;
  geo = buttonbox->geometry;
  for( j = 0; j < geo->box_rows; j++ ) {
    for( i = 0; i < geo->box_cols; i++ ) {
      x = btn_wdth * i;
      y = btn_hght * j;
      buttonbox->buttons[btn].display = display;
      buttonbox->buttons[btn].gc = gc;
      buttonbox->buttons[btn].wndwID =
	XCreateSimpleWindow (display, wndwID, x, y, btn_wdth, btn_hght,
			     (unsigned int)0, (unsigned long)0, background);
      if( (buttonbox->buttons[btn].feel->nfunctions >0) &&
	  (buttonbox->buttons[btn].feel->function[0] != BTNNoOp) ) {
#if VMS && IMTOOL
	XSelectAsyncInput(display, buttonbox->buttons[btn].wndwID,
			  ButtonReleaseMask | ButtonPressMask |
			  EnterWindowMask | LeaveWindowMask | ExposureMask,
			  XZ_ast, XZ_efn);
#endif
	XSelectInput(display, buttonbox->buttons[btn].wndwID,
		     ButtonReleaseMask | ButtonPressMask |
		     EnterWindowMask | LeaveWindowMask | ExposureMask);
      /* no_op buttons have no events, but are mapped */
      } else {
#if VMS && IMTOOL
	XSelectAsyncInput(display, buttonbox->buttons[btn].wndwID,
			  ExposureMask | ButtonPressMask | ButtonReleaseMask,
			  XZ_ast, XZ_efn);
#endif
	XSelectInput(display, buttonbox->buttons[btn].wndwID,
		     ExposureMask | ButtonPressMask | ButtonReleaseMask);
      }
      /* put the window id in the buttonbox's check event list */
      buttonbox->window_list[btn] = buttonbox->buttons[btn].wndwID;
      if( ++btn >= buttonbox->btn_cnt )
	return;
    }
  }
}

/*
 * Subroutine:	btn_LabelButtons
 * Purpose:	Create XImages for all states of all buttons.
 * Returns:	void
 * Called by:	MakeButtonBox() above, ResizeBox() in ResizeBox.c
 * Uses:	btn_Alloc() in BtnAlloc.c
 * Uses:	btn_MakeBtdBitmap() in MakeBtnBdr.c
 * Uses:	btn_MakeXImages() in MakeXImage.c
 * Xlib calls:	none
 * Post-state:	box record has XImages filled for all buttons
 * Method:	Creates basic pattern for button minus label which is used as
 *		the starting point for adding labels to make each button.
 */
void btn_LabelButtons ( buttonbox, btn_wdth, btn_hght, off, on )
     ButtonBox buttonbox;	/* i: structure for boxed group of buttons */
     int btn_wdth, btn_hght;	/* i: dimensions of each button */
     int off, on;		/* i: flag set if off or on has invert bits */
{
  int byte_width;		/* l: width in bytes of the button bitmap */
  int mapsz;			/* l: size of button bitmap in bytes */
  int btn_cnt;			/* l: number of buttons specificied */
  int i;			/* l: loop counter */
  Visual *visual;		/* l: visual for XCreateImage() */
  unsigned char *off_out;	/* l: filled bitmap with only border pattern */
  unsigned char *off_in;	/* l: filled bitmap with only border pattern */
  unsigned char *on_out;	/* l: filled bitmap with only border pattern */
  unsigned char *on_in;		/* l: filled bitmap with only border pattern */
  char *btn_Alloc();
  void btn_MakeBdrBitmap(), btn_MakeXImages();

  /* calculate number of bytes in one line and in whole bitmap */
  byte_width = (btn_wdth + 7) / 8;
  mapsz = btn_hght * byte_width;
  /* allocate space for patterns (initially empty) */
  off_out = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button border");
  off_in = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button border");
  on_out = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button border");
  on_in = (unsigned char *)btn_Alloc(mapsz, sizeof(char), "button border");
  /* if border patterns were given, fill the border in */
  if( buttonbox->borders != NULL ) {
    btn_MakeBdrBitmap(off_out, btn_wdth, btn_hght, byte_width,
		      buttonbox->borders->off_out, off);
    btn_MakeBdrBitmap(off_in, btn_wdth, btn_hght, byte_width,
		      buttonbox->borders->off_in, off);
    btn_MakeBdrBitmap(on_out, btn_wdth, btn_hght, byte_width,
		      buttonbox->borders->on_out, on);
    btn_MakeBdrBitmap(on_in, btn_wdth, btn_hght, byte_width,
		      buttonbox->borders->on_in, on);
  }
  visual = buttonbox->visual;
  btn_cnt = buttonbox->btn_cnt;
  for( i = 0; i < btn_cnt; i++ ) {
    btn_MakeXImages(&(buttonbox->buttons[i]), btn_wdth, btn_hght, byte_width,
		    visual, off_out, off_in, on_out, on_in, mapsz,
		    off, on, motf_look);
  }
  /* free the patterns */
  free((char *)off_out);
  free((char *)off_in);
  free((char *)on_out);
  free((char *)on_in);
}
              
/*
 * Subroutine:	SetTAEButtonLook
 * Purpose:	set flag prior to execution of any code to make buttons
 *		look more like TAE (or monochrome motif) buttons
 */
void SetTAEButtonLook ( status )
     int status;
{
  motf_look = status;
}
