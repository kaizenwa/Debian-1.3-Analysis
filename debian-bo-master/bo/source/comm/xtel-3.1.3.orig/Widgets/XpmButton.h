/*	
 *   Copyright (C) 1995  Pierre Ficheux (pierre@rd.lectra.fr)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ident "$Id: XpmButton.h,v 1.1 1996/09/14 22:27:47 pierre Exp $"

#ifndef _XpmButton_h
#define _XpmButton_h

#include <X11/Xaw/Simple.h>

/* XpmButton widget resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 pixmap2	     Pixmap		Pixmap		None
 pixmap1	     Pixmap             Pixmap		None
 shape2	     	     Pixshape		Pixshape	None
 shape1	     	     Pixshape           Pixshape	None
 shadowColor1	     ShadowColor	Pixel		DefaultForeground
 shadowColor2	     ShadowColor	Pixel		DefaultForeground
 shadowThickness     ShadowThickness	Dimension	0
 helpString	     HelpString		String		NULL
 helpForeground	     HelpForeground	Pixel		DefaultForeground
 helpBackground	     HelpBackground	Pixel		DefaultBackground
 helpFont	     Font		XFontStruct *	XtDefaultFont
 unsensitiveBitmap   Pixmap		Pixmap		None
 callback	     Callback		XtCallbackList	NULL
*/

#define XtNpixmap2		"pixmap2"
#define XtNpixmap1		"pixmap1"
#define XtNshape2 		"shape2"
#define XtNshape1 		"shape1"
#define XtNunsensitiveBitmap 	"unsensitiveBitmap"
#define XtNhelpFont		"helpFont"
#define XtNhelpString		"helpString"
#define XtNhelpBackground	"helpBackground"
#define XtNhelpForeground	"helpForeground"
#define XtNshadowColor1		"shadowColor1"
#define XtNshadowColor2		"shadowColor2"
#define XtNshadowThickness	"shadowThickness"

#define XtCPixshape 		"Pixshape"
#define XtCHelpString		"HelpString"
#define XtRPixshape 		"Pixshape"
#define XtCShadowColor		"ShadowColor"
#define XtCShadowThickness	"ShadowThickness"
#define XtCHelpBackground	"HelpBackground"
#define XtCHelpForeground	"HelpForeground"

extern WidgetClass     		xpmButtonWidgetClass;

typedef struct _XpmButtonClassRec   *XpmButtonWidgetClass;
typedef struct _XpmButtonRec        *XpmButtonWidget;

typedef Pixmap Pixshape;

#endif /* _XpmButton_h */
/* DON'T ADD STUFF AFTER THIS */
