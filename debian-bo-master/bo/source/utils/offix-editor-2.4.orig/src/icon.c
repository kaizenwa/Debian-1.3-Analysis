/*
 *  Editor - a menu-driven text editor
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
#include "xedit.h"
#include "icon.h"

#ifdef XPM
#include <X11/xpm.h>
#include "editor.xpm"
#else
#include "editor.xbm"
#include "editor_mask.xbm"
#endif

static Pixmap	EditorIcon,EditorMask;

void
ReadIcons(Widget toplevel)
{
	XWMHints *hints;
	Display *dpy=XtDisplay(toplevel);
	int Width,Height;
	int scrn=DefaultScreen(dpy);
	int dpth=XDefaultDepth(dpy,scrn);
	Window win=DefaultRootWindow(dpy);
	/* Create the pixmaps accordingly		       *
	 * All the pixmaps MUST have the same height and width */
#ifdef XPM
	XpmAttributes xpmattributes;
	xpmattributes.depth=dpth;
	xpmattributes.valuemask=XpmSize | XpmDepth;
	XpmCreatePixmapFromData(dpy,win,editor_xpm,&EditorIcon,&EditorMask,
				&xpmattributes);
	Width=xpmattributes.width;
	Height=xpmattributes.height;
#else
	EditorIcon=XCreatePixmapFromBitmapData(dpy,win,editor_bits,
			editor_width,editor_height,BlackPixel(dpy,scrn),
			WhitePixel(dpy,scrn),dpth);
	EditorMask=XCreateBitmapFromData(dpy, win,editor_mask_bits,
			editor_mask_width,editor_mask_height);
	Width=editor_width;
	Height=editor_height;
#endif
	
	/* Now tell the window manager what the icon is */
	hints=XAllocWMHints();
	hints->flags=IconPixmapHint | IconMaskHint;
	hints->icon_pixmap=EditorIcon;
	hints->icon_mask=EditorMask;
	XSetWMHints(dpy,XtWindow(toplevel),hints);
	XFree(hints);
}
