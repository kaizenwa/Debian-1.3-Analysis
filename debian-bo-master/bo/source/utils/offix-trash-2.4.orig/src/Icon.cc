/*
 *  Trash - the OffiX waste basket
 *  Copyright (C) 1996  César Crusius
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#include <X11/Intrinsic.h>
#include <OffiX/DragAndDrop.h>

#ifdef XSHAPE
	#include <X11/extensions/shape.h>
#endif

#include <stdio.h>

#ifdef XPM
#include <X11/xpm.h>
#include "trash.xpm"
#include "trash_full.xpm"
#else
#include "trash.xbm"
#include "trash_mask.xbm"
#include "trash_full.xbm"
#include "trash_full_mask.xbm"
#endif

extern Display *dpy;
extern Widget toplevel,InfoLabel;

static Pixmap	EmptyTrashIcon,EmptyTrashMask;
static Pixmap	FullTrashIcon,FullTrashMask;
static Window IconWindow;
static int LastFlag;

void ExposeEventHandler(Widget widget,XtPointer data,XEvent *event,Boolean* p);

void
ReadIcons(void)
{
	XSetWindowAttributes attributes;
	unsigned long valuemask=0;
	Window win=DefaultRootWindow(dpy);
	int Width=32,Height=32;
	int scrn=DefaultScreen(dpy);
	int dpth=XDefaultDepth(dpy,scrn);
	
	/* Now create the icon window */
	IconWindow=XCreateWindow(dpy,win,0,0,Width,Height,0,
				 CopyFromParent,CopyFromParent,
				 CopyFromParent,valuemask,&attributes);

	/* Now tell the window manager that the icon window is ours */
	XWMHints *hints=XAllocWMHints();
	hints->flags=IconWindowHint | StateHint ;
	hints->icon_window=IconWindow;
	hints->initial_state=IconicState;
	XSetWMHints(dpy,XtWindow(toplevel),hints);
	XFree(hints);
    
	/* Create the pixmaps accordingly		       *
	 * All the pixmaps MUST have the same height and width */
#ifdef XPM
	XpmAttributes xpmattributes;
	xpmattributes.depth=dpth;
	xpmattributes.valuemask=XpmSize | XpmDepth;
	XpmCreatePixmapFromData(dpy,win,trash,&EmptyTrashIcon,&EmptyTrashMask,
				&xpmattributes);
	XpmCreatePixmapFromData(dpy,win,trash_full,&FullTrashIcon,
				&FullTrashMask,&xpmattributes);
	Width=xpmattributes.width;
	Height=xpmattributes.height;
#else
	EmptyTrashIcon=XCreatePixmapFromBitmapData(dpy,win,trash_bits,
			trash_width,trash_height,BlackPixel(dpy,scrn),
			WhitePixel(dpy,scrn),dpth);
	EmptyTrashMask=XCreateBitmapFromData(dpy, win,trash_mask_bits,
			trash_mask_width,trash_mask_height);
	FullTrashIcon=XCreatePixmapFromBitmapData(dpy, win,trash_full_bits,
			trash_full_width,trash_full_height,
			BlackPixel(dpy,scrn),WhitePixel(dpy,scrn),dpth);
	FullTrashMask=XCreateBitmapFromData(dpy, win,trash_full_mask_bits,
			trash_full_mask_width,trash_full_mask_height);
	Width=trash_width;
	Height=trash_height;
#endif

#ifdef HAVE_XTREGISTERDRAWABLE
	/* Register the icon window to a non-toplevel widget	*
	 * This widget must be drop-registered in order to	*
	 * receive icon drops.					*/
	XtRegisterDrawable(dpy,IconWindow,InfoLabel);
#endif
}

void
UpdateIcon(int Flag)
{
	Pixmap *icon,*mask;
	
	LastFlag=Flag;
	if(Flag) { icon=&FullTrashIcon;  mask=&FullTrashMask; }
	else	 { icon=&EmptyTrashIcon; mask=&EmptyTrashMask; }

#ifdef XSHAPE
	XShapeCombineMask(dpy,IconWindow,ShapeBounding,0,0,
			  *mask,ShapeSet);
#endif
	XSetWindowBackgroundPixmap(dpy,IconWindow,*icon);
	XClearWindow(dpy,IconWindow);
}
