/*
 *  Clipboard - a DND compatible xclipboard
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
#include "icon.h"

#ifdef XPM
#include <X11/xpm.h>
#include "clipboard.xpm"
#else
#include "clipboard.xbm"
#include "clipboard_mask.xbm"
#endif

static Pixmap	ClipboardIcon,ClipboardMask;

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
	XpmCreatePixmapFromData(dpy,win,clipboard_xpm,&ClipboardIcon,&ClipboardMask,
				&xpmattributes);
	Width=xpmattributes.width;
	Height=xpmattributes.height;
#else
	ClipboardIcon=XCreatePixmapFromBitmapData(dpy,win,clipboard_bits,
			clipboard_width,clipboard_height,BlackPixel(dpy,scrn),
			WhitePixel(dpy,scrn),dpth);
	ClipboardMask=XCreateBitmapFromData(dpy, win,clipboard_mask_bits,
			clipboard_mask_width,clipboard_mask_height);
	Width=clipboard_width;
	Height=clipboard_height;
#endif
	
	/* Now tell the window manager what the icon is */
	hints=XAllocWMHints();
	hints->flags=IconPixmapHint | IconMaskHint;
	hints->icon_pixmap=ClipboardIcon;
	hints->icon_mask=ClipboardMask;
	XSetWMHints(dpy,XtWindow(toplevel),hints);
	XFree(hints);
}
