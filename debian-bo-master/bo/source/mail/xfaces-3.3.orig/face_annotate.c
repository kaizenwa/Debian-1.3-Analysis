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
 * Last Modified On: Mon Mar  7 18:02:58 1994
 * Update Count    : 285
 * Status          : Released
 * 
 * HISTORY
 * 6-Mar-1994		Chris Liebman	
 *    Last Modified: Sun Mar  6 12:28:00 1994 #274 (Chris Liebman)
 *    Fixed a problem with FaceAnnotateWork being called with a struct
 *    rather than a pointer to a struct!
 *
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sun Feb 13 00:38:41 1994 #271 (Chris Liebman)
 *    Make images that are smaller than the tile size bigger and
 *    Allow choice as to weather the extra height is on the top or the
 *    bottom.  The image is always centered left to right.
 * 
 * PURPOSE
 * 	Handle image annotation.
*/

#ifndef lint
static char *RCSid = "$Id: face_annotate.c,v 1.4 1994/03/08 02:16:44 liebman Exp $";
#endif

#include "faces.h"
#include "face_image.h"

typedef struct annotation Annotation;

struct annotation
{
    Position		x;
    Position		y;
    Dimension		max_width;
    Pixel		fg;
    Pixel		bg;
    XFontStruct*	font;
    Boolean		shape_text;
    Boolean		opaque_text;
};


#define	offset(field)	XtOffset(Annotation*, field)

static XtResource AnnotationResourcesList[] =
{
    {
	"x", "Position", XtRPosition, sizeof(Position),
	offset(x), XtRString, "0"
    },
    {
	"y", "Position", XtRPosition, sizeof(Position),
	offset(y), XtRString, "0"
    },
    {
	"maxWidth", "Dimension", XtRDimension, sizeof(Dimension),
	offset(max_width), XtRString, "48"
    },
    {
	"foreground", "Foreground", XtRPixel, sizeof(Pixel),
	offset(fg), XtRString, XtDefaultForeground
    },
    {
	"background", "Background", XtRPixel, sizeof(Pixel),
	offset(bg), XtRString, XtDefaultBackground
    },
    {
	"font", "Font", XtRFontStruct, sizeof(XFontStruct *),
	offset(font), XtRString, XtDefaultFont
    },
    {
	"shapeText", "ShapeText", XtRBoolean, sizeof(Boolean),
	offset(shape_text), XtRString, "False"
    },
    {
	"opaqueText", "OpaqueText", XtRBoolean, sizeof(Boolean),
	offset(opaque_text), XtRString, "True"
    },
};

#undef offset

static int		annotationCount = 0;
static Annotation*	annotations = NULL;

static int		unknownAnnotationCount = 0;
static Annotation*	unknownAnnotations = NULL;

static void
FaceAnnotateWork(face, a, astr, imageWidth, imageHeight, imageGC, shapeGC)
Face*		face;
Annotation*	a;
char*		astr;
unsigned int	imageWidth;
unsigned int	imageHeight;
GC		imageGC;
GC		shapeGC;
{
    int			length;
    int			dir, ascent, descent;
    int			x, y;
    int			width, height;
    int			maxWidth;
    XCharStruct		size;
    Display*		display = XtDisplay(TheTopLevel);
    
    if (astr == NULL || *astr == '\0')
    {
	return;
    }
    
    /*
     *  Compute max allowable width for text.
    */
    
    x = a->x;
    y = a->y;
    
    if (x < 0)
    {
	maxWidth = imageWidth + x;
    }
    else
    {
	maxWidth = imageWidth - x;
    }
    
    if (maxWidth > a->max_width)
    {
	maxWidth = a->max_width;
    }

    /*
     *  Compute the size of the string.
    */
    
    size.width = imageWidth + 1;
    for(length = strlen(astr); length > 0; --length)
    {
	XTextExtents(a->font, astr, length, &dir, &ascent, &descent, &size);
	
	if (size.width <= maxWidth)
	{
	    break;
	}
    }
    
    if (length == 0)
    {
	length = 1;
    }
    
    width  = size.width;
    height = ascent + descent;
    
    /*
     * Upper left corner of text.
    */
    
    if (x < 0)
    {
	x = imageWidth - width + x;
    }
    
    if (y < 0)
    {
	y = imageHeight - height + y;
    }
    
    /*
     * Set shape mask area for label if shaped.
    */
    
    if (face->shape != None)
    {
	if (a->shape_text)
	{
	    XDrawString(display, face->shape, shapeGC,
			x, y+ascent, astr, length);
	}
	else
	{
	    XFillRectangle(display, face->shape, shapeGC,
			   x, y, width, height);
	}
    }
    
    if (a->opaque_text)
    {
	XDrawImageString(display, face->pixmap, imageGC,
			 x, y+ascent, astr, length);
    }
    else
    {
	XDrawString(display, face->pixmap, imageGC,
			 x, y+ascent, astr, length);
    }
}

void
FaceAnnotateFree(face)
Face *face;
{
    int i;
    
    if (face->annotations == NULL)
    {
	face->pixmap = None;
	face->shape  = None;
	return;
    }
    
    for(i = 0; face->annotations[i]; ++i)
    {
	XtFree(face->annotations[i]);
    }
    
    XtFree((char*)face->annotations);
    
    if (face->pixmap != None)
    {
	XFreePixmap(XtDisplay(TheTopLevel), face->pixmap);
    }
    
    if (face->shape != None)
    {
	XFreePixmap(XtDisplay(TheTopLevel), face->shape);
    }
    
    face->annotations = NULL;
    face->pixmap      = None;
    face->shape       = None;
}

/*
 *   Return 1 if image changed!
*/

int
FaceAnnotate(face)
Face*	face;
{
    Pixmap		pixmap;
    Pixmap		shape;
    unsigned int	imageWidth;
    unsigned int	imageHeight;
    int			imageDepth;
    Window		imageRoot;
    unsigned int	shapeWidth;
    unsigned int	shapeHeight;
    int			shapeDepth;
    Window		shapeRoot;
    int			junkx, junky;
    unsigned int	junkbw;
    unsigned int	mask;
    XGCValues		values;
    GC			imageGC;
    GC			shapeGC;
    Display*		display = XtDisplay(TheTopLevel);
    int			i;
    int			x = 0;
    int			y = 0;
    char**		adata;
    Dimension		tileHeight;
    Dimension		tileWidth;
    int			extraHeight = 0;
    int			extraWidth = 0;
    int			count;
    Annotation*		annos;

    if (face->items->item->unknown)
    {
	count = unknownAnnotationCount;
	annos = unknownAnnotations;
    }
    else
    {
	count = annotationCount;
	annos = annotations;
    }
    
    /*
     *  We use the newest mail item to do the annotations.
    */
    
    adata = face->items->item->annotations;
    
    /*
     *  If there is no "new" annotation data now then just clear it all off.
    */
    
    if (adata == NULL)
    {
 	pixmap = face->pixmap;
 	FaceAnnotateFree(face);
 	face->pixmap = FaceImagePixmap(face->image);
 	face->shape  = FaceImageShape(face->image);
  	
 	return (pixmap != face->pixmap);
    }
    
    /*
     *  If there is old annotation data then see if it the same as the
     * new data.  If it is then we need to do nothing!
    */
    
    if (face->annotations != NULL)
    {
	
	for(i = 0;  i < count; ++i)
	{
	    if ((face->annotations[i] == NULL) ||
		(adata[i]             == NULL) ||
		(strcmp(face->annotations[i], adata[i]) != 0))
	    {
		break;
	    }
	}
	
	/*
	 * Nothing changed!
	*/
	
	if ((i >= count) ||
	    ((face->annotations[i] == NULL) && (adata[i] == NULL)))
	{
	    return 0;
	}
    }
    
    /*
     *  Ok, things have changed, free the old annotations.
    */
    
    FaceAnnotateFree(face);
    
    /*
     *    Now make the annotations.
    */
    
    if ((pixmap = FaceImagePixmap(face->image)) == None)
    {
	return 0;
    }
    
    shape  = FaceImageShape(face->image);
    
    /*
     *  We may need to make the pixmaps bigger to accomidate annotation
     * either above or below.  Here we compute the position of the old
     * pixmap data and the extra height of the new.
    */
    
    XtVaGetValues(TheFrame,
		  XtNtileWidth,  &tileWidth,
		  XtNtileHeight, &tileHeight, NULL);
    
    /*
     *   Copy the pixmap and any shape.
    */
    
    XGetGeometry(display, pixmap, &imageRoot, &junkx, &junky,
		 &imageWidth, &imageHeight, &junkbw, &imageDepth);
    
    /*
     * If the image is smaller than the frame need to enlarge it.
    */
    
    if (tileHeight > imageHeight)
    {
	extraHeight = tileHeight - imageHeight;
	if (TheFacesResources.annotation_above)
	{
	    y = extraHeight;
	}
    }
    
    if (tileWidth > imageWidth)
    {
	extraWidth = tileWidth - imageWidth;
	x = extraWidth / 2;
    }
    
    face->pixmap = XCreatePixmap(display, imageRoot,
				 imageWidth  + extraWidth,
				 imageHeight + extraHeight,
				 imageDepth);
    
    /*
     * Create a GC to use with the image.
    */
    
    mask = GCForeground | GCFunction | GCGraphicsExposures | GCFillStyle;
    
    values.foreground         = TheFacesResources.background;
    values.function           = GXcopy;
    values.fill_style         = FillSolid;
    values.graphics_exposures = False;
    
    imageGC = XCreateGC(display, face->pixmap, mask, &values);
    
    /*
     * Copy the image.
    */
    
    XFillRectangle(display, face->pixmap, imageGC,
		   0, 0,
		   imageWidth  + extraWidth,
		   imageHeight + extraHeight);
    
    XCopyArea(display, pixmap, face->pixmap, imageGC,
	      0, 0, imageWidth, imageHeight, x, y);
    
    if (shape == None)
    {
	shapeWidth  = imageWidth;
	shapeHeight = imageHeight;
	shapeRoot   = imageRoot;
	shapeDepth  = 1;
    }
    else
    {
	XGetGeometry(display, shape, &shapeRoot, &junkx, &junky,
		     &shapeWidth, &shapeHeight, &junkbw, &shapeDepth);
    }
    
    face->shape = XCreatePixmap(display, shapeRoot,
				shapeWidth  + extraWidth,
				shapeHeight + extraHeight,
				shapeDepth);
    
    /*
     *   Create a gc for use with the shape.
    */
    
    mask = (GCForeground | GCBackground | GCFunction |
	    GCFillStyle  | GCGraphicsExposures);
    
    values.foreground         = 0;
    values.background         = 0;
    values.function           = GXcopy;
    values.fill_style         = FillSolid;
    values.graphics_exposures = False;
    
    shapeGC = XCreateGC(display, face->shape, mask, &values);
    
    /*
     *  Copy shape data.
    */

    XFillRectangle(display, face->shape, shapeGC,
		   0, 0,
		   shapeWidth  + extraWidth,
		   shapeHeight + extraHeight);
    
    XSetForeground(display, shapeGC, 1);
    
    if (shape == None)
    {
	if (!TheFacesResources.shape_extra)
	{
	    shapeWidth  += extraWidth;
	    shapeHeight += extraHeight;
	    x = 0;
	    y = 0;
	}
	
	XFillRectangle(display, face->shape, shapeGC,
		       x, y, shapeWidth, shapeHeight);
    }
    else
    {
	XCopyArea(display, shape, face->shape, shapeGC,
		  0, 0, shapeWidth, shapeHeight, x, y);
    }
    
    /*
     *   Now for each valid annotation.
    */
    
    face->annotations = (char**)XtCalloc(count+1, sizeof(char*));
    
    for(i = 0; i < count; ++i)
    {
	/*
	 *  Stop if we run out of data.
	*/
	
	if (adata[i] == NULL)
	{
	    face->annotations[i] = NULL;
	    break;
	}
	
	/*
	 *  Copy the string.
	*/
	
	face->annotations[i] = XtNewString(adata[i]);
	
	/*
	 * Annotate image!
	*/
	
	XSetFont(display, imageGC, annos[i].font->fid);
	
	if (face->shape != None)
	{
	    XSetFont(display, shapeGC, annos[i].font->fid);
	}
	
	XSetForeground(display, imageGC, annos[i].fg);
	XSetBackground(display, imageGC, annos[i].bg);
	
	FaceAnnotateWork(face, &annos[i], face->annotations[i],
			 imageWidth  + extraWidth,
			 imageHeight + extraHeight,
			 imageGC, shapeGC);
    }
    
    XFreeGC(display, imageGC);
    
    if (face->shape)
    {
	XFreeGC(display, shapeGC);
    }
    
    return 1;
}

void
FaceAnnotateInit()
{
    int			i;
    char		name[30];
    
    annotationCount = TheFacesResources.annotation_count;
    annotations = (Annotation*) XtCalloc(annotationCount, sizeof(Annotation));
    
    for(i = 0; i < annotationCount; ++i)
    {
	sprintf(name, "annotation%d", i+1);
	
	XtGetSubresources(TheTopLevel, &annotations[i], name, "Annotation",
			  AnnotationResourcesList,
			  XtNumber(AnnotationResourcesList),
			  NULL, 0);
    };
    
    unknownAnnotationCount = TheFacesResources.unknown_annotation_count;
    unknownAnnotations = (Annotation*) XtCalloc(unknownAnnotationCount,
						sizeof(Annotation));
    
    for(i = 0; i < unknownAnnotationCount; ++i)
    {
	sprintf(name, "unknownAnnotation%d", i+1);
	
	XtGetSubresources(TheTopLevel, &unknownAnnotations[i],
			  name, "Annotation",
			  AnnotationResourcesList,
			  XtNumber(AnnotationResourcesList),
			  NULL, 0);
    };
}

