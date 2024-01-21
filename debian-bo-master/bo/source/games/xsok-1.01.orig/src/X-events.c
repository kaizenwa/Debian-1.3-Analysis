/*****************************************************************************/
/*									     */
/*									     */
/*	Xsok version 1.00 -- module X-events.c				     */
/*									     */
/*	Event handler functions for the X window system.		     */
/*	Written by Michael Bischoff (mbi@mo.math.nat.tu-bs.de)		     */
/*	November-1994							     */
/*	see COPYRIGHT.xsok for Copyright details			     */
/*									     */
/*									     */
/*****************************************************************************/
#include "X-sok.h"
#include <X11/keysym.h>		/* X11 key code definitions */

void refresh_screen(void) {
    XClearArea(dpy, table, 0, 0, 0, 0, True);
}

/* event entry points are: key_press, button_press, button_release, redraw_table */
int mouse_x = 0, mouse_y = 0;

void button_press(XButtonPressedEvent *xev) {
    mouse_x = xev->x / DX;
    mouse_y = xev->y / DY;
    switch (xev->button) {
    case Button1:            /* quick move */
	key_pressed("Mouse1");
	break;
    case Button2:            /* quick move */
	key_pressed("Mouse2");
	break;
    case Button3:            /* quick move */
	key_pressed("Mouse3");
	break;
    case Button4:            /* quick move */
	key_pressed("Mouse4");
	break;
    case Button5:            /* quick move */
	key_pressed("Mouse5");
	break;
    }
}


void key_press(XKeyPressedEvent *xev) {
    char str[32];
    int num;

#define	get_name_field()	get_selection()

    num = XKeycodeToKeysym(dpy, xev->keycode, 0);

    if (num & 0xff00) {
    	switch (num) {
    	case XK_Up:
	    key_pressed("Up");
	    return;
    	case XK_Left:
	    key_pressed("Left");
	    return;
    	case XK_Down:
	    key_pressed("Down");
	    return;
    	case XK_Right:
	    key_pressed("Right");
	    return;
    	case XK_Return:
    	case XK_Linefeed:
	    key_pressed("\n");
	    return;
    	case XK_BackSpace:
    	case XK_Delete:
	    key_pressed("\b");
	    return;
    	case XK_Escape:
	    key_pressed("\033");
	    return;
	}
        return;
    }

    num = XLookupString(xev, str, 31, NULL, NULL);
    if (num == 0)
	return;
    str[num] = '\0';		/* NULL to terminate it */

    key_pressed(str);
}

/*****************************************************************************/
/*									     */
/*	Functions for resize events and resize requests			     */
/*									     */
/*****************************************************************************/

/* 1) hard resizes (i.e. forcing the outer window to change size) */
/*    I think these are not liked in the Xaw community */

void cmd_Resize(void) {
    XSize_t w, h;
    w = graphic.width;
    h = graphic.height;
    Force_Resize(w, h);
}

/* event handler function. This function is called by the Widget in response
   to a request from us. In Xaw, this is a resize of the logical area, i.e.
   of the virtual size of the tableau. */

void resize_event(XSize_t w, XSize_t h) {
#ifdef LABER
    printf("resize event to (%d,%d) called\n", w, h);
#endif
    if (graphic.height == h && graphic.width == w)
	return;		/* no change of size */

    /* in xlib, we must clear the new area by hand; there may be illegal data
       left in the server. This applies to Xaw as well */
    {   XExposeEvent xev;
	xev.count = -1;
        if (gamegraphic) {
	    if (graphic.height < h) {
		/* window is greater now */
		XClearArea(dpy, table, 0, graphic.height, graphic.width, h - graphic.height, True);
		++xev.count;
	    }
	    if (graphic.width < w) {
		/* window is greater now */
		XClearArea(dpy, table, graphic.width, 0, w - graphic.width, h, True);
		++xev.count;
	    }
	    if (xev.count >= 0) {
		/* generate synthetic expose events for the new area */
		/* this must be done before we possibly change the layout */
		if (graphic.height < h) {
		    /* window is greater now */
		    xev.x = 0;
		    xev.y = graphic.height;
		    xev.width = graphic.width;
		    xev.height = h - graphic.height;
		    redraw_table(&xev);
		    --xev.count;
		}
		if (graphic.width < w) {
		    /* window is greater now */
		    xev.x = graphic.width;
		    xev.y = 0;
		    xev.width = w - graphic.width;
		    xev.height = h;
		    redraw_table(&xev);
		}
	    }
	}
    }
    graphic.height = h;
    graphic.width = w;

    if (!gamegraphic)
	return;
}

