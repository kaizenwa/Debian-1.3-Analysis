/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : 
 * Last Modified By: Chris Liebman
 * Last Modified On: 
 * Update Count    : 0
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Simple tiled layout widget
 *
 * $Id: Tiled.h,v 1.2 1994/02/08 18:59:37 liebman Exp $
*/

#ifndef Tiled_h_
#define Tiled_h_

/*
 * Tiled Widget
*/

#include <X11/Intrinsic.h>
#include <X11/Composite.h>

#define	XtNhorizSpacing	"horizSpacing"
#define	XtNvertSpacing	"vertSpacing"
#define	XtNtileWidth	"tileWidth"
#define	XtNtileHeight	"tileHeight"
#define	XtNsetWidth	"setWidth"
#define	XtNsetHeight	"setHeight"
#define	XtNminWidth	"minWidth"
#define	XtNminHeight	"minHeight"
#define	XtNmaxWidth	"maxWidth"
#define	XtNmaxHeight	"maxHeight"
#define XtNlayout	"layout"

#define XtCLayout	"Layout"

/* Class record constants */

extern WidgetClass tiledWidgetClass;

typedef struct _TiledClassRec *TiledWidgetClass;
typedef struct _TiledRec      *TiledWidget;

#endif /* Tiled_h_ */
