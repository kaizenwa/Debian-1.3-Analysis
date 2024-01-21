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
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sun Feb 13 17:10:53 1994
 * Update Count    : 30
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Tue Feb  1 08:14:16 1994 #29 (Chris Liebman)
 *    Use shape pixmap from face if available.
 *
 * 1-Feb-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 23:07:23 1994 #24 (Chris Liebman)
 *    Added a hack to handle the case where the mask (an the image pixmap
 *    are smaller than the widget.  In this case the Label widget centers
 *    the image so we attempt to center the shape mask.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 23:05:49 1994 #21 (Chris Liebman)
 *    More shape support improvements.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 17:40:55 1994 #12 (Chris Liebman)
 *    Much improved shape support.
 * 
 * PURPOSE
 * 	Shape the faces window to the combined image masks.
*/

#ifndef lint
static char *RCSid = "$Id: face_shape.c,v 1.10 1994/02/13 22:33:30 liebman Exp $";
#endif

#include "faces.h"
#include <X11/extensions/shape.h>

static Boolean	initialized = False;
static GC	FaceShapeGC;
static Pixmap	FaceShapePixmap;

static void
FaceShapeFace(widget, shape)
Widget		widget;
Pixmap		shape;
{
    Dimension		width = 0;
    Dimension		height = 0;
    Dimension		borderWidth;
    Dimension		internalWidth;
    Dimension		internalHeight;
    Position		x;
    Position		y;
    int			maskX;
    int			maskY;
    Window		child;
    
    /*
     *    Get the origin of this widget.
    */
    
    XtVaGetValues(widget, XtNx, &x, XtNy, &y,
		  XtNwidth, &width, XtNheight, &height,
		  XtNborderWidth, &borderWidth,
		  XtNinternalHeight, &internalHeight,
		  XtNinternalWidth, &internalWidth,
		  NULL);
    
    /*
     *  If we have no shape then we shape the window to inside the borders
     * or to inside the internal width and height.
    */
    
    if (shape == None)
    {
	if (TheFacesResources.shape_borders)
	{
	    x += borderWidth;
	    y += borderWidth;
	    
	    if (TheFacesResources.shape_internal)
	    {
		x += internalWidth;
		y += internalHeight;
		width  -= 2 * internalWidth;
		height -= 2 * internalHeight;
	    }
	}
	else
	{
	    x -= borderWidth;
	    y -= borderWidth;
	    width  += 2 * borderWidth;
	    height += 2 * borderWidth;
	}
    }
    else
    {
	x += borderWidth;
	y += borderWidth;
    }
    
    /*
     *   Now convert to choords in terms of the top level shell
     * window.
    */
    
    XTranslateCoordinates(XtDisplay(TheTopLevel), XtWindow(TheFrame),
			  XtWindow(TheTopLevel), x, y, &maskX, &maskY,
			  &child);
    
    /*
     *    If this widget has a shape then apply it.
     *  Otherwise set use the size info to set the shape mask.
    */
    
    if (shape != None)
    {
	/*
	 *  We need this bit of code until we have a real "tiled" widget
	 * as the frame.
	*/
	
	Window	junkroot;
	int junkx, junky;
	unsigned int junkbw, junkdepth;
	unsigned int shapeWidth;
	unsigned int shapeHeight;
	int width_diff, height_diff;
	
	XGetGeometry(XtDisplay(TheTopLevel), shape,
		     &junkroot, &junkx, &junky,
		     &shapeWidth, &shapeHeight,
		     &junkbw, &junkdepth);
	
	width_diff  = width  - shapeWidth;
	height_diff = height - shapeHeight;
	
	/*
	 *  Note: We only adjust when the widget width is greater than the
	 * shape width!
	*/
	
	if (width_diff > 0)
	{
	    maskX += width_diff / 2;
	    width = shapeWidth;
	}
	
	if (height_diff > 0)
	{
	    maskY += height_diff / 2;
	    height = shapeHeight;
	}
	
	/*
	 *    Copy in this part of the shape.
	*/
	
	XCopyArea(XtDisplay(TheTopLevel), shape, FaceShapePixmap,
		  FaceShapeGC,
		  0, 0, width, height,
		  maskX, maskY);
    }
    else
    {
	XFillRectangle(XtDisplay(TheTopLevel), FaceShapePixmap,
		       FaceShapeGC, maskX, maskY, width, height);
    }
}

void
FaceShapeCreate()
{
    Dimension		width = 0;
    Dimension		height = 0;
    XGCValues		values;
    Face		*face;
    XWindowAttributes	attributes;
    
    if (TheFacesResources.use_shape != True)
    {
	return;
    }
    
    if (FaceShapePixmap != None)
    {
	XFreePixmap(XtDisplay(TheFrame), FaceShapePixmap);
	FaceShapePixmap = None;
    }
    
    /*
     *   We have to get the window attributes as a get values will still get 
     * old information untill we start processing events!
    */
    
    XGetWindowAttributes(XtDisplay(TheTopLevel), XtWindow(TheTopLevel),
			 &attributes);
    width  = attributes.width;
    height = attributes.height;
    
    FaceShapePixmap = XCreatePixmap(XtDisplay(TheTopLevel),
				    XtWindow(TheTopLevel),
				    width, height, 1);
    
    /*
     *   If we have not done it yet, create a graphic context.
    */
    
    if (initialized != True)
    {
	FaceShapeGC = XCreateGC(XtDisplay(TheTopLevel), FaceShapePixmap,
				0, &values);
	initialized = True;
    }
    
    /*
     *   Set shape mask to all zeros.
    */
    
    XSetForeground(XtDisplay(TheTopLevel), FaceShapeGC, 0);
    XFillRectangle(XtDisplay(TheTopLevel), FaceShapePixmap, FaceShapeGC,
		   0, 0, width, height);
    
    /*
     *   Now we will use ones to shape the window.
    */
    
    XSetForeground(XtDisplay(TheTopLevel), FaceShapeGC, 1);
    
    /*
     *    Now go thru the face list and apply any shape masks.
    */
    
    for (face = TheFaceList; face != NULL; face = face->next)
    {
	FaceShapeFace(face->widget, face->shape);
    }
    
    /*
     *    If there are no faces in the list then set the shape mask 
     * to all ones.
    */
    
    if (TheFaceList == NULL)
    {
	FaceShapeFace(NoMailWidget, FaceImageShape(NoMailImage));
    }
    
    /*
     *    Ok, now shape the window, I hope!
    */
    
    XShapeCombineMask(XtDisplay(TheTopLevel), XtWindow(TheTopLevel),
		      ShapeBounding, 0, 0, FaceShapePixmap, ShapeSet);
}

