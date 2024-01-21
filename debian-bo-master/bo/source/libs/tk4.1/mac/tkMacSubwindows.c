/* 
 * tkMacSubwindows.c --
 *
 *	Implements subwindows for the macintosh version of Tk.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * SCCS: @(#) tkMacSubwindows.c 1.48 96/07/23 16:55:09
 */

#include "tkInt.h"
#include "X.h"
#include "Xlib.h"
#include <stdio.h>

#include <Windows.h>
#include <QDOffscreen.h>
#include "tkMacInt.h"

/*
 * Hash table for Mac Window -> TkWindow mapping.
 */

static Tcl_HashTable windowTable;
static int windowHashInit = false;

/*
 * Temporary region that can be reused.
 */
static RgnHandle tmpRgn = NULL;

void TkMacWMInitBounds _ANSI_ARGS_((TkWindow *winPtr, Rect *geometry));
static void MacMoveWindow _ANSI_ARGS_((WindowRef window, int x, int y));
static void UpdateOffsets _ANSI_ARGS_((TkWindow *winPtr, int deltaX, int deltaY));


/*
 *----------------------------------------------------------------------
 *
 * TkMacGetXWindow --
 *
 *	Returns the X window Id associated with the given WindowRef.
 *
 * Results:
 *	The window id is returned.  None is returned if not a Tk window.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Window
TkMacGetXWindow(macWinPtr)
    WindowRef macWinPtr;
{
    register Tcl_HashEntry *hPtr;

    if ((macWinPtr == NULL) || !windowHashInit) {
	return None;
    }
    hPtr = Tcl_FindHashEntry(&windowTable, (char *) macWinPtr);
    if (hPtr == NULL) {
	return None;
    }
    return (Window) Tcl_GetHashValue(hPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * TkMakeWindow --
 *
 *	Creates an X Window (Mac subwindow).
 *
 * Results:
 *	The window id is returned.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

Window
TkMakeWindow(winPtr, parent)
    TkWindow *winPtr;
    Window parent;
{
    Rect geometry;
    MacDrawable *macWin;
    XEvent event;

    /*
     * Allocate sub window
     */
    macWin = (MacDrawable *) ckalloc(sizeof(MacDrawable));
    if (macWin == NULL) {
	winPtr->privatePtr = NULL;
	return None;
    }
    macWin->winPtr = winPtr;
    winPtr->privatePtr = macWin;
    macWin->clipRgn = NewRgn();
    macWin->aboveClipRgn = NewRgn();
    macWin->referenceCount = 0;
    macWin->flags = TK_CLIP_INVALID;
    macWin->scrollWinPtr = NULL;

    if (Tk_IsTopLevel(macWin->winPtr)) {
	WindowRef newWindow = NULL;
	short windowType;
	Tcl_HashEntry *valueHashPtr;
	int new;

	TkMacWMInitBounds(winPtr, &geometry);
	
	if (winPtr->atts.override_redirect == true ||
		TkGetTransientMaster(macWin->winPtr) != None) {
	    windowType = plainDBox;
	} else {
	    windowType = documentProc;
	}

	newWindow = NewCWindow(NULL, &geometry, "\ptemp", false, 
		windowType, (WindowRef) -1, true, 0);
	if (newWindow == NULL) {
	    return None;
	}

	MacMoveWindow(newWindow, (int) geometry.left, (int) geometry.top);
	SetPort((GrafPtr) newWindow);

	macWin->flags |= TK_NEVER_MAPPED;
	macWin->portPtr = (GWorldPtr) newWindow;
	macWin->toplevel = macWin;
	macWin->xOff = 0;
	macWin->yOff = 0;
	
	/*
	 * Add macWin pointer to hash table of Mac windows.
	 * We can use the hash table to find Tk data structures
	 * from an incoming Mac window.
	 */
	if (!windowHashInit) {
	    Tcl_InitHashTable(&windowTable, TCL_ONE_WORD_KEYS);
	    windowHashInit = true;
	}
	valueHashPtr = Tcl_CreateHashEntry(&windowTable,
		(char *) newWindow, &new);
	if (!new) {
	    panic("same macintosh window allocated twice!");
	}
	Tcl_SetHashValue(valueHashPtr, macWin);
    } else {
	macWin->portPtr = NULL;
	macWin->xOff = winPtr->parentPtr->privatePtr->xOff +
	    winPtr->parentPtr->changes.border_width +
	    winPtr->changes.x;
	macWin->yOff = winPtr->parentPtr->privatePtr->yOff +
	    winPtr->parentPtr->changes.border_width +
	    winPtr->changes.y;
	macWin->toplevel = winPtr->parentPtr->privatePtr->toplevel;
    }

    macWin->toplevel->referenceCount++;
    
    /* 
     * TODO: need general solution for visibility events.
     */
    event.xany.serial = Tk_Display(winPtr)->request;
    event.xany.send_event = False;
    event.xany.display = Tk_Display(winPtr);
	
    event.xvisibility.type = VisibilityNotify;
    event.xvisibility.window = (Window) macWin;;
    event.xvisibility.state = VisibilityUnobscured;
    Tk_QueueWindowEvent(&event, TCL_QUEUE_TAIL);

    return (Window) macWin;
}

/*
 *----------------------------------------------------------------------
 *
 * XDestroyWindow --
 *
 *	Dealocates the given X Window.
 *
 * Results:
 *	The window id is returned.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void 
XDestroyWindow(Display *display, Window window)
{
    MacDrawable *macWin = (MacDrawable *) window;
    GWorldPtr destPort;

    /*
     * Remove any dangling pointers that may exist if
     * the window we are deleting is being tracked by
     * the grab code.
     */
    TkMacPointerDeadWindow(macWin->winPtr);
    TkMacSetScrollbarGrow(macWin->winPtr, false);
    destPort = TkMacGetDrawablePort(window);
    macWin->toplevel->referenceCount--;
    
    if (Tk_IsTopLevel(macWin->winPtr)) {
	DisposeRgn(macWin->clipRgn);
	DisposeRgn(macWin->aboveClipRgn);
	macWin->portPtr = NULL;
	if (macWin->toplevel->referenceCount == 0) {
	    ckfree((char *) macWin->toplevel);
	}
	
	/*
	 * Delete the Mac window and remove it from the windowTable.
	 */
	Tcl_DeleteHashEntry(Tcl_FindHashEntry(&windowTable,
		(char *) destPort));	
	DisposeWindow((WindowRef) destPort);
    } else {
	if (destPort != NULL) {
	    SetGWorld(destPort, NULL);
	    InvalRgn(macWin->aboveClipRgn); /* TODO: this may not be valid */
	}
	if (macWin->winPtr->parentPtr != NULL) {
	    InvalClipRgns(macWin->winPtr->parentPtr);
	}
	DisposeRgn(macWin->clipRgn);
	DisposeRgn(macWin->aboveClipRgn);
	if (macWin->toplevel->referenceCount == 0) {
	    ckfree((char *) macWin->toplevel);
	}
	ckfree((char *) macWin);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XMapWindow --
 *
 *	Map the given X Window to the screen.  See X window documentation 
 *  for more details.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The subwindow or toplevel may appear on the screen.
 *
 *----------------------------------------------------------------------
 */

void 
XMapWindow(display, window)
    Display *display;
    Window window;
{
    MacDrawable *macWin = (MacDrawable *) window;
    XEvent event;
    GWorldPtr destPort;

    destPort = TkMacGetDrawablePort(window);

    if (Tk_IsTopLevel(macWin->winPtr)) {
	ShowWindow((WindowRef) destPort);

	/*
	 * If this is the first time this toplevel is to be
	 * mapped we need to send a configure event for the
	 * window.
	 */
	if (macWin->flags | TK_NEVER_MAPPED) {
	    Point where = {0, 0};
	    int xOffset, yOffset;
	    
	    macWin->flags &= ~TK_NEVER_MAPPED;
	    LocalToGlobal(&where);
	    TkMacWindowOffset((WindowRef) destPort, &xOffset, &yOffset);
	    where.h -= xOffset;
	    where.v -= yOffset;
	    TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, 
		where.h, where.v, -1, -1, TK_LOCATION_CHANGED);
	}

	/* 
	 * We only need to send the MapNotify event
	 * for toplevel windows.
	 */
	event.xany.serial = display->request;
	event.xany.send_event = False;
	event.xany.display = display;
	
	event.xmap.window = window;
	event.xmap.type = MapNotify;
	event.xmap.event = window;
	event.xmap.override_redirect = macWin->winPtr->atts.override_redirect;
	Tk_QueueWindowEvent(&event, TCL_QUEUE_TAIL);
    } else {
	InvalClipRgns(macWin->winPtr->parentPtr);
    }

    /* 
     * Generate damage for that area of the window 
     */
    SetGWorld(destPort, NULL);
    TkMacUpdateClipRgn(macWin->winPtr);
    InvalRgn(macWin->aboveClipRgn);
}

/*
 *----------------------------------------------------------------------
 *
 * XUnmapWindow --
 *
 *	Unmap the given X Window to the screen.  See X window
 *	documentation for more details.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The subwindow or toplevel may be removed from the screen.
 *
 *----------------------------------------------------------------------
 */

void 
XUnmapWindow(display, window)
    Display *display;
    Window window;
{
    MacDrawable *macWin = (MacDrawable *) window;
    XEvent event;
    GWorldPtr destPort;

    destPort = TkMacGetDrawablePort(window);

    if (Tk_IsTopLevel(macWin->winPtr)) {
	HideWindow((WindowRef) destPort);

	/* 
	 * We only need to send the UnmapNotify event
	 * for toplevel windows.
	 */
	event.xany.serial = display->request;
	event.xany.send_event = False;
	event.xany.display = display;
	
	event.xunmap.type = UnmapNotify;
	event.xunmap.window = window;
	event.xunmap.event = window;
	event.xunmap.from_configure = false;
	Tk_QueueWindowEvent(&event, TCL_QUEUE_TAIL);
    } else {
	/* 
	 * Generate damage for that area of the window.
	 */
	SetGWorld(destPort, NULL);
	InvalRgn(macWin->aboveClipRgn); /* TODO: may not be valid */
	InvalClipRgns(macWin->winPtr->parentPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XResizeWindow --
 *
 *	Resize a given X window.  See X windows documentation for
 *	further details.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void 
XResizeWindow(display, window, width, height)
    Display *display;
    Window window; 
    unsigned int width;
    unsigned int height;
{
    MacDrawable *macWin = (MacDrawable *) window;
    GWorldPtr destPort;

    destPort = TkMacGetDrawablePort(window);

    display->request++;
    SetPort((GrafPtr) destPort);
    if (Tk_IsTopLevel(macWin->winPtr)) {
	/* 
	 * NOTE: we are not adding the new space to the update
	 * regoin.  It is currently assumed that Tk will need
	 * to completely redraw anway.
	 */
	SizeWindow((WindowRef) destPort, (short) width, (short) height, false);
	InvalRgn(macWin->clipRgn);
	InvalClipRgns(macWin->winPtr);
    } else {
	/* TODO: update all xOff & yOffs */
	int deltaX, deltaY;
	MacDrawable *macParent = macWin->winPtr->parentPtr->privatePtr;

	InvalClipRgns(macWin->winPtr->parentPtr);

	deltaX = - macWin->xOff;
	deltaY = - macWin->yOff;
	deltaX += macParent->xOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.x;
	deltaY += macParent->yOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.y;
	UpdateOffsets(macWin->winPtr, deltaX, deltaY);
    }
    TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, -1, -1, 
	macWin->winPtr->changes.width, macWin->winPtr->changes.height, TK_SIZE_CHANGED);
}

/*
 *----------------------------------------------------------------------
 *
 * XMoveResizeWindow --
 *
 *	Move or resize a given X window.  See X windows documentation
 *	for further details.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void 
XMoveResizeWindow(Display *display, Window window, 
	int x, int y,
	unsigned int width, unsigned int height)
{	
    MacDrawable *macWin = (MacDrawable *) window;
    GWorldPtr destPort;

    destPort = TkMacGetDrawablePort(window);

    SetPort((GrafPtr) destPort);
    if (Tk_IsTopLevel(macWin->winPtr)) {
	/* 
	 * NOTE: we are not adding the new space to the update
	 * regoin.  It is currently assumed that Tk will need
	 * to completely redraw anway.
	 */
	SizeWindow((WindowRef) destPort, (short) width, (short) height, false);
	MacMoveWindow((WindowRef) destPort, x, y);

	/* TODO: is the following right? */
	InvalRgn(macWin->clipRgn);
	InvalClipRgns(macWin->winPtr);
	TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, x, y, width, height, TK_BOTH_CHANGED);
    } else {
	int deltaX, deltaY;
	Rect bounds;
	MacDrawable *macParent = macWin->winPtr->parentPtr->privatePtr;
	if (macParent == NULL) {
	    return; /* TODO: Probably should be a panic */
	}

	if (!EmptyRgn(macWin->clipRgn)) {
	    InvalRgn(macWin->clipRgn);
	}
	InvalClipRgns(macWin->winPtr->parentPtr);

	deltaX = - macWin->xOff;
	deltaY = - macWin->yOff;
	deltaX += macParent->xOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.x;
	deltaY += macParent->yOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.y;
		
	UpdateOffsets(macWin->winPtr, deltaX, deltaY);
	TkMacWinBounds(macWin->winPtr, &bounds);
	InvalRect(&bounds);
	TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, 
	    macWin->winPtr->changes.x, macWin->winPtr->changes.y, 
	    width, height, TK_BOTH_CHANGED);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XMoveWindow --
 *
 *	Move a given X window.  See X windows documentation for further
 *  details.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void 
XMoveWindow(display, window, x, y)
    Display* display;
    Window window;
    int x;
    int y;
{
    MacDrawable *macWin = (MacDrawable *) window;
    GWorldPtr destPort;

    destPort = TkMacGetDrawablePort(window);

    SetPort((GrafPtr) destPort);
    if (Tk_IsTopLevel(macWin->winPtr)) {
	/* 
	 * NOTE: we are not adding the new space to the update
	 * regoin.  It is currently assumed that Tk will need
	 * to completely redraw anway.
	 */
	MacMoveWindow((WindowRef) destPort, x, y);

	/* TODO: is the following right? */
	InvalRgn(macWin->clipRgn);
	InvalClipRgns(macWin->winPtr);
	TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, x, y, -1, -1, TK_LOCATION_CHANGED);
    } else {
	int deltaX, deltaY;
	Rect bounds;
	MacDrawable *macParent = macWin->winPtr->parentPtr->privatePtr;
	if (macParent == NULL) {
	    return; /* TODO: Probably should be a panic */
	}

	if (!EmptyRgn(macWin->clipRgn)) {
	    InvalRgn(macWin->clipRgn);
	}
	InvalClipRgns(macWin->winPtr->parentPtr);

	deltaX = - macWin->xOff;
	deltaY = - macWin->yOff;
	deltaX += macParent->xOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.x;
	deltaY += macParent->yOff +
	    macWin->winPtr->parentPtr->changes.border_width +
	    macWin->winPtr->changes.y;
		
	UpdateOffsets(macWin->winPtr, deltaX, deltaY);
	TkMacWinBounds(macWin->winPtr, &bounds);
	InvalRect(&bounds);
	TkGenWMConfigureEvent((Tk_Window) macWin->winPtr, 
	    macWin->winPtr->changes.x, macWin->winPtr->changes.y, -1, -1, TK_LOCATION_CHANGED);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XRaiseWindow --
 *
 *	Change the stacking order of a window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Changes the stacking order of the specified window.
 *
 *----------------------------------------------------------------------
 */

void 
XRaiseWindow(display, window)
    Display* display;
    Window window;
{
    MacDrawable *macWin = (MacDrawable *) window;
    
    display->request++;
    if (Tk_IsTopLevel(macWin->winPtr)) {
	TkWmRestackToplevel(macWin->winPtr, Above, NULL);
    } else {
    	/* TODO: this should generate damage */
    }
}

/*
 *----------------------------------------------------------------------
 *
 * XConfigureWindow --
 *
 *	Change the size, position, stacking, or border of the specified
 *	window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Changes the attributes of the specified window.  Note that we
 *	ignore the passed in values and use the values stored in the
 *	TkWindow data structure.
 *
 *----------------------------------------------------------------------
 */

void
XConfigureWindow(display, w, value_mask, values)
    Display* display;
    Window w;
    unsigned int value_mask;
    XWindowChanges* values;
{
    MacDrawable *macWin = (MacDrawable *) w;
    TkWindow *winPtr = macWin->winPtr;

    display->request++;

    /*
     * Change the shape and/or position of the window.
     */

    if (value_mask & (CWX|CWY|CWWidth|CWHeight)) {
	XMoveResizeWindow(display, w, winPtr->changes.x, winPtr->changes.y,
		winPtr->changes.width, winPtr->changes.height);
    }

    /*
     * Change the stacking order of the window.  Tk actuall keeps all
     * the information we need for stacking order.  All we need to do
     * is make sure the clipping regions get updated and generate damage
     * that will ensure things get drawn correctly.
     */

    if (value_mask & CWStackMode) {
	Rect bounds;
	GWorldPtr destPort;
	
	destPort = TkMacGetDrawablePort(w);
	SetPort((GrafPtr) destPort);
	InvalClipRgns(winPtr->parentPtr);
	TkMacWinBounds(winPtr, &bounds);
	InvalRect(&bounds);
    } 

    /* TkGenWMMoveRequestEvent(macWin->winPtr, 
	    macWin->winPtr->changes.x, macWin->winPtr->changes.y); */
}

/*
 *----------------------------------------------------------------------
 *
 *  TkMacUpdateClipRgn --
 *
 *	This function updates the cliping regions for a given window
 *	and all of its children.  Once updated the TK_CLIP_INVALID flag
 *	in the subwindow data structure is unset.  The TK_CLIP_INVALID 
 *	flag should always be unset before any drawing is attempted.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The clip regions for the window and its children are updated.
 *
 *----------------------------------------------------------------------
 */

void
TkMacUpdateClipRgn(winPtr)
    TkWindow *winPtr;
{
    RgnHandle rgn;
    int x, y;
    TkWindow *win2Ptr;

    if (winPtr == NULL) {
	return;
    }
    
    if (winPtr->privatePtr->flags & TK_CLIP_INVALID) {
	rgn = winPtr->privatePtr->aboveClipRgn;
	if (tmpRgn == NULL) {
	    tmpRgn = NewRgn();
	}
	
	/* 
	 * Start with a region defined by the window bounds.
	 */
	x = winPtr->privatePtr->xOff;
	y = winPtr->privatePtr->yOff;
	SetRectRgn(rgn, (short) x, (short) y,
		(short) (winPtr->changes.width  + x), 
		(short) (winPtr->changes.height + y));

	/* 
	 * Clip away the area of any windows that may obscure this
	 * window.  First, clip to the parents visable clip region.
	 * Second, clip away any siblings that are higher in the
	 * stacking order.
	 */
	if (!Tk_IsTopLevel(winPtr)) {
	    TkMacUpdateClipRgn(winPtr->parentPtr);
	    SectRgn(rgn, 
		    winPtr->parentPtr->privatePtr->aboveClipRgn, rgn);
				
	    win2Ptr = winPtr->nextPtr;
	    while (win2Ptr != NULL) {
		if (Tk_IsTopLevel(win2Ptr) || !Tk_IsMapped(win2Ptr)) {
		    win2Ptr = win2Ptr->nextPtr;
		    continue;
		}
		x = win2Ptr->privatePtr->xOff;
		y = win2Ptr->privatePtr->yOff;
		SetRectRgn(tmpRgn, (short) x, (short) y,
			(short) (win2Ptr->changes.width  + x), 
			(short) (win2Ptr->changes.height + y));
		DiffRgn(rgn, tmpRgn, rgn);
							  
		win2Ptr = win2Ptr->nextPtr;
	    }
	}
		
	/* 
	 * The final clip region is the aboveClip region (or visable
	 * region) minus all the children of this window.
	 */
	rgn = winPtr->privatePtr->clipRgn;
	CopyRgn(winPtr->privatePtr->aboveClipRgn, rgn);
		
	win2Ptr = winPtr->childList;
	while (win2Ptr != NULL) {
	    if (Tk_IsTopLevel(win2Ptr) || !Tk_IsMapped(win2Ptr)) {
		win2Ptr = win2Ptr->nextPtr;
		continue;
	    }
	    x = win2Ptr->privatePtr->xOff;
	    y = win2Ptr->privatePtr->yOff;
	    SetRectRgn(tmpRgn, (short) x, (short) y,
		    (short) (win2Ptr->changes.width  + x), 
		    (short) (win2Ptr->changes.height + y));
	    DiffRgn(rgn, tmpRgn, rgn);
							  
	    win2Ptr = win2Ptr->nextPtr;
	}
		
	winPtr->privatePtr->flags &= ~TK_CLIP_INVALID;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TkMacVisableClipRgn --
 *
 *	This function returnd the Macintosh cliping region for the 
 *	given window.  A NULL Rgn means the window is not visable.
 *
 * Results:
 *	The region.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

RgnHandle
TkMacVisableClipRgn(winPtr)
    TkWindow *winPtr;
{
    if (winPtr->privatePtr->flags & TK_CLIP_INVALID) {
	TkMacUpdateClipRgn(winPtr);
    }

    return winPtr->privatePtr->clipRgn;
}

/*
 *----------------------------------------------------------------------
 *
 * TkMacGetDrawablePort --
 *
 *	This function returns the Graphics Port for a given X drawable.
 *
 * Results:
 *	A GWorld pointer.  Either an off screen pixmap or a Window.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

GWorldPtr
TkMacGetDrawablePort(drawable)
    Drawable drawable;
{
    MacDrawable *macWin = (MacDrawable *) drawable;
    
    if (macWin->clipRgn == NULL) {
	return macWin->portPtr;
    }
    
    return macWin->toplevel->portPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * InvalClipRgns --
 *
 *	This function invalidates the clipping regions for a given
 *	window and all of its children.  This function should be
 *	called whenever changes are made to subwindows that would
 *	effect the size or position of windows.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The cliping regions for the window and its children are
 *	mark invalid.  (Make sure they are valid before drawing.)
 *
 *----------------------------------------------------------------------
 */

void
InvalClipRgns(winPtr)
    TkWindow *winPtr;
{
    TkWindow *childPtr;
	
    /* 
     * If already marked we can stop because all 
     * decendants will also already be marked.
     */
    if (winPtr->privatePtr->flags & TK_CLIP_INVALID) {
	return;
    }
	
    winPtr->privatePtr->flags |= TK_CLIP_INVALID;
	
    /* 
     * Invalidate clip regions for all children & 
     * thier decendants - unless the child is a toplevel.
     */
    childPtr = winPtr->childList;
    while (childPtr != NULL) {
	if (!Tk_IsTopLevel(childPtr) && Tk_IsMapped(childPtr)) {
	    InvalClipRgns(childPtr);
	}
	childPtr = childPtr->nextPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TkMacWinBounds --
 *
 *	Given a Tk window this function determines the windows
 *	bounds in relation to the Macintosh window's coordinate
 *	system.  This is also the same coordinate system as the
 *	Tk toplevel window in which this window is contained.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

void
TkMacWinBounds(winPtr, bounds)
    TkWindow *winPtr;
    Rect *bounds;
{
    bounds->left = (short) winPtr->privatePtr->xOff;
    bounds->top = (short) winPtr->privatePtr->yOff;
    bounds->right = (short) (winPtr->privatePtr->xOff +
	    winPtr->changes.width);
    bounds->bottom = (short) (winPtr->privatePtr->yOff +
	    winPtr->changes.height);
}

/*
 *----------------------------------------------------------------------
 *
 * MacMoveWindow --
 *
 *	A replacement for the Macintosh MoveWindow function.  This
 *	function adjusts the inputs to MoveWindow to offset the root of 
 *	the window system.  This has the effect of making the coords 
 *	refer to the window dressing rather than the top of the content.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Moves the Macintosh window.
 *
 *----------------------------------------------------------------------
 */

static void 
MacMoveWindow(WindowRef window, int x, int y)
{
    int xOffset, yOffset;

    TkMacWindowOffset(window, &xOffset, &yOffset);
    MoveWindow((WindowRef) window, 
	(short) (x + xOffset), (short) (y + yOffset), false);
}

/*
 *----------------------------------------------------------------------
 *
 * UpdateOffsets --
 *
 *	Updates the X & Y offsets of the given TkWindow from the
 *	TopLevel it is a decendant of.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The xOff & yOff fields for the Mac window datastructure
 *	is updated to the proper offset.
 *
 *----------------------------------------------------------------------
 */

static void
UpdateOffsets(winPtr, deltaX, deltaY)
    TkWindow *winPtr;
    int deltaX, deltaY;
{
    TkWindow *childPtr;

    winPtr->privatePtr->xOff += deltaX;
    winPtr->privatePtr->yOff += deltaY;

    childPtr = winPtr->childList;
    while (childPtr != NULL) {
	if (!Tk_IsTopLevel(childPtr)) {
	    UpdateOffsets(childPtr, deltaX, deltaY);
	}
	childPtr = childPtr->nextPtr;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_GetPixmap --
 *
 *	Creates an in memory drawing surface.
 *
 * Results:
 *	Returns a handle to a new pixmap.
 *
 * Side effects:
 *	Allocates a new Macintosh GWorld.
 *
 *----------------------------------------------------------------------
 */

Pixmap
Tk_GetPixmap(display, d, width, height, depth)
    Display *display;		/* Display for new pixmap. */
    Drawable d;			/* Drawable where pixmap will be used. */
    unsigned int width, height;	/* Dimensions of pixmap. */
    unsigned int depth;		/* Bits per pixel for pixmap. */
{
    QDErr err;
    GWorldPtr gWorld;
    Rect bounds;
    MacDrawable *macPix;
    PixMapHandle pixels;
    
    display->request++;
    macPix = (MacDrawable *) ckalloc(sizeof(MacDrawable));
    macPix->winPtr = NULL;
    macPix->xOff = 0;
    macPix->yOff = 0;
    macPix->clipRgn = NULL;
    macPix->aboveClipRgn = NULL;
    macPix->referenceCount = 0;
    macPix->toplevel = NULL;
    macPix->flags = 0;
    macPix->scrollWinPtr = NULL;

    bounds.top = bounds.left = 0;
    bounds.right = (short) width;
    bounds.bottom = (short) height;
    err = NewGWorld(&gWorld, 0, &bounds, NULL, NULL, 0);
    if (err != noErr) {
        panic("NewGWorld failed in XCreatePixmap");
    }

    /*
     * TODO: This is a short term solution.  We should Lock 
     * the pixels only when we need to draw.
     */
    pixels = GetGWorldPixMap(gWorld);
    LockPixels(pixels);
    macPix->portPtr = gWorld;

    return (Pixmap) macPix;
}

/*
 *----------------------------------------------------------------------
 *
 * Tk_FreePixmap --
 *
 *	Release the resources associated with a pixmap.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Deletes the Macintosh GWorld created by Tk_GetPixmap.
 *
 *----------------------------------------------------------------------
 */

void 
Tk_FreePixmap(display, pixmap)
    Display *display;		/* Display. */
    Pixmap pixmap;     		/* Pixmap to destroy */
{
    MacDrawable *macPix = (MacDrawable *) pixmap;
    PixMapHandle pixels;

    display->request++;
    pixels = GetGWorldPixMap(macPix->portPtr);
    UnlockPixels(pixels);
    DisposeGWorld(macPix->portPtr);
    ckfree((char *) macPix);
}

/*
 *----------------------------------------------------------------------
 *
 * TkMacSetScrollbarGrow --
 *
 *	Sets a flag for a toplevel window indicating that the passed
 *	Tk scrollbar window will display the grow region for the 
 *	toplevel window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A flag is set int windows toplevel parent.
 *
 *----------------------------------------------------------------------
 */

void 
TkMacSetScrollbarGrow(winPtr, flag)
    TkWindow *winPtr;		/* Tk scrollbar window. */
    int flag;			/* Boolean value true or false. */
{
    if (flag) {
	winPtr->privatePtr->toplevel->flags |= TK_SCROLLBAR_GROW;
	winPtr->privatePtr->toplevel->scrollWinPtr = winPtr;
    } else if (winPtr->privatePtr->toplevel->scrollWinPtr == winPtr) {
	winPtr->privatePtr->toplevel->flags &= ~TK_SCROLLBAR_GROW;
	winPtr->privatePtr->toplevel->scrollWinPtr = NULL;	
    }
}

/*
 *----------------------------------------------------------------------
 *
 * TkMacGetScrollbarGrowWindow --
 *
 *	Tests to see if a given window's toplevel window contains a
 *	scrollbar that will draw the GrowIcon for the window.
 *
 * Results:
 *	Boolean value.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

TkWindow * 
TkMacGetScrollbarGrowWindow(winPtr)
    TkWindow *winPtr;	/* Tk window. */
{
    if (winPtr == NULL) return NULL;
    return winPtr->privatePtr->toplevel->scrollWinPtr;
}
