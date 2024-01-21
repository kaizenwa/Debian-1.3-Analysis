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
 * Last Modified On: Sun Feb 20 11:54:38 1994
 * Update Count    : 13
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Mon Jan 31 22:46:53 1994 #9 (Chris Liebman)
 *    Automaticly make the bitmap deeper if possible.
 *
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Thu Jan 27 14:35:24 1994 #8 (Chris Liebman)
 *    Added new init function/sequence.
 * 
 * PURPOSE
 * 	Handle xbm images.
*/

#ifndef lint
static char *RCSid = "$Id: face_image_xbm.c,v 1.7 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"
#include "face_image.h"

typedef struct face_image_xbm
{
    Pixmap		pixmap;
    Pixmap		shape;
} FaceImageXbm;

typedef struct face_xbm_resources FaceXbmResources;

struct face_xbm_resources
{
    Pixel	fg;
    Pixel	bg;
};

#define	offset(field)	XtOffset(FaceXbmResources*, field)

static XtResource FaceXbmResourcesList[] =
{
    {
	"foreground", "Foreground", XtRPixel, sizeof(Pixel),
	offset(fg), XtRString, XtDefaultForeground
    },
    {
	"background", "Background", XtRPixel, sizeof(Pixel),
	offset(bg), XtRString, XtDefaultBackground
    }
};

static Display	*display;
static Window	root;
static GC gc;
static FaceXbmResources TheXbmResources;
static int depth;

#define	XBM_EXT		".xbm"
#define	XBM_SHAPE_EXT	".xbm-shape"

void
FaceImageColorize(pixmap, width, height)
Pixmap* pixmap;
int	width;
int	height;
{
    /*
     *   Make the image deeper than one if it looks possible.
    */
    
    if (depth > 1)
    {
	Pixmap		color;
	
	color = XCreatePixmap(display, root, width, height, depth);
	
	XCopyPlane(display, *pixmap, color, gc,
		   0, 0, width, height, 0, 0, 1);
	
	XFreePixmap(display, *pixmap);
	*pixmap = color;
    }
}

/*
 *    Read an xbm file.
*/

void *
FaceImageXbmRead(file, type_data)
String	file;
void*	type_data;
{
    Pixmap		pixmap;
    int			width;
    int			height;
    int			depth;
    FaceImageXbm	*fib;
    
    if (XReadBitmapFile(display, root, file,
			&width, &height, &pixmap,
			NULL, NULL) != BitmapSuccess)
    {
	return(NULL);
    }
    
    FaceImageColorize(&pixmap, width, height);
    
    fib = (FaceImageXbm *)XtCalloc(1, sizeof(FaceImageXbm));
    
    fib->pixmap     = pixmap;
    fib->shape	    = None;
    
    return((void *)fib);
}

#ifdef SHAPE
/*
 *    Read a shaped xbm file.
*/

void *
FaceImageShapedXbmRead(file, type_data)
String	file;
void*	type_data;
{
    Pixmap		pixmap;
    Pixmap		shape;
    int			width;
    int			height;
    String		filename;
    FaceImageXbm	*fib;
    
    if (TheFacesResources.use_shape != True)
    {
	return(NULL);
    }
    
    if (XReadBitmapFile(XtDisplay(TheFrame),
			DefaultRootWindow(XtDisplay(TheFrame)), file,
			&width, &height, &pixmap,
			NULL, NULL) != BitmapSuccess)
    {
	return(NULL);
    }
    
    filename = StringConcat(file, "-mask");
    
    if (XReadBitmapFile(XtDisplay(TheFrame),
			DefaultRootWindow(XtDisplay(TheFrame)), filename,
			&width, &height, &shape,
			NULL, NULL) != BitmapSuccess)
    {
	XFreePixmap(XtDisplay(TheFrame), pixmap);
	XtFree(filename);
	return(NULL);
    }
    
    XtFree(filename);
    
    fib = (FaceImageXbm *)XtCalloc(1, sizeof(FaceImageXbm));
    
    fib->pixmap     = pixmap;
    fib->shape	    = shape;
    
    return((void *)fib);
}
#endif

/*
 *    Free an xbm file.
*/

void
FaceImageXbmFree(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXbm	*fib = data;
    
    if (fib->pixmap != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fib->pixmap);
    }
    
    if (fib->shape != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fib->shape);
    }
    
    XtFree((void *)fib);
}

/*
 *    Return pixmap for xbm file.
*/

Pixmap
FaceImageXbmPixmap(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXbm	*fib = data;
    
    return(fib->pixmap);
}

/*
 *    Return shape for xbm file.
*/

Pixmap
FaceImageXbmShape(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXbm	*fib = data;
    
    return(fib->shape);
}

static FaceImageType FaceImageTypeXbm = 
{
    "xbm",
    FaceImageXbmRead,
    FaceImageXbmFree,
    FaceImageXbmPixmap,
    FaceImageXbmShape,
    ".xbm",
    NULL,
    NULL
};

#ifdef SHAPE
static FaceImageType FaceImageTypeShapedXbm = 
{
    "xbm-shape",
    FaceImageShapedXbmRead,
    FaceImageXbmFree,
    FaceImageXbmPixmap,
    FaceImageXbmShape,
    "-shape.xbm",
    NULL,
    NULL
};
#endif

void
FaceImageXbmInit()
{
    unsigned int	mask;
    XGCValues		values;
    
    FaceImageTypeRegister(&FaceImageTypeXbm);
#ifdef SHAPE
    FaceImageTypeRegister(&FaceImageTypeShapedXbm);
#endif
    
    display = XtDisplay(TheTopLevel);
    root = DefaultRootWindow(display);
    
    XtVaGetValues(TheTopLevel, XtNdepth, &depth, NULL);
    
    XtGetSubresources(TheTopLevel, &TheXbmResources, "xbm", "Xbm",
		      FaceXbmResourcesList,
		      XtNumber(FaceXbmResourcesList),
		      NULL, 0);
    
    mask = (GCForeground | GCBackground | GCFunction |
	    GCGraphicsExposures | GCFillStyle);
    
    values.foreground         = TheXbmResources.fg;
    values.background         = TheXbmResources.bg;
    values.function           = GXcopy;
    values.fill_style         = FillSolid;
    values.graphics_exposures = False;
    
    gc = XCreateGC(display, root, mask, &values);
}
