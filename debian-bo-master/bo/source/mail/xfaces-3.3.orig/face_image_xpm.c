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
 * Last Modified On: Sun Feb 20 15:30:58 1994
 * Update Count    : 66
 * Status          : Released
 * 
 * HISTORY
 * 31-Jan-1994		Chris Liebman	
 *    Last Modified: Sat Jan 29 22:57:30 1994 #22 (Chris Liebman)
 *    Added new init function/sequence. Removed previous color flag as
 *    you can configure the image types that are attempted.
 *
 * 24-Jan-1994		Chris Liebman	
 *    Last Modified: Sun Jan 23 15:01:11 1994 #12 (Chris Liebman)
 *    Added check to allow the disabling of colors.
 *
 * 18-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 16:51:51 1994 #3 (Chris Liebman)
 *    Added closeness value from Martin.Kraemer@mch.sni.de
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:22:25 1994 #1 (Chris Liebman)
 *    moved include of xpm header file here from faces.h as no other file
 *    needs it.
 * 
 * PURPOSE
 * 	Handle xpm images.
*/

#ifndef lint
static char *RCSid = "$Id: face_image_xpm.c,v 1.11 1994/03/12 21:19:48 liebman Exp $";
#endif

#include "faces.h"
#include "face_image.h"
#include <X11/xpm.h>
#include <X11/Xproto.h>     /* get X_FreeColors id */

typedef struct face_image_xpm
{
    Pixmap		pixmap;
    Pixmap		shape;
    XpmAttributes	*attributes;
} FaceImageXpm;


typedef struct face_xpm_filter
{
    String    name;
    String    filter;
    String    extension;
} FaceXpmFilter;

#define       offset(field)   XtOffset(FaceXpmFilter*, field)
#define       offset(field)   XtOffset(FaceXpmFilter*, field)

static XtResource FaceXpmFilterResourcesList[] =
{
    {
      "name", "Name", XtRString, sizeof(String),
      offset(name), XtRString, NULL
    },
    {
      "filter", "Filter", XtRString, sizeof(String),
      offset(filter), XtRString, NULL
    },
    {
      "extension", "Extension", XtRString, sizeof(String),
      offset(extension), XtRString, NULL
    }
};

#undef offset


typedef struct face_xpm_resources
{
    Boolean	override_none_color;
    String	none_color;
    int		filter_count;
} FaceXpmResources;

#define	offset(field)	XtOffset(FaceXpmResources*, field)

static XtResource FaceXpmResourcesList[] =
{
    {
	"noneColor", "NoneColor", XtRString, sizeof(String),
	offset(none_color), XtRString, XtDefaultBackground
    },
    {
	"overrideNoneColor", "OverrideNoneColor", XtRBoolean, sizeof(Boolean),
	offset(override_none_color), XtRString, "False"
    },
    {
	"filterCount", "FilterCount", XtRInt, sizeof(int),
        offset(filter_count), XtRString, "0"
    }
};

#undef offset

static FaceXpmResources	TheXpmResources;
static Colormap		colormap;
static Display*		display;
static Window		root;

static XpmAttributes*
FaceImageXpmAttributes(shaped)
Boolean shaped;
{
    XpmAttributes*	attrs;
    XpmColorSymbol*	csym = NULL;
    
    /*
     *    Create the attribute struct and throw in the colormap to use.
    */
    
    attrs = (XpmAttributes *)XtCalloc(1, XpmAttributesSize());
    
#ifdef XpmCloseness
    /*
     * If the Closness attribute is supported then use it.
    */
    
    attrs->valuemask = XpmReturnPixels | XpmColormap | XpmCloseness;
    
    /*
     * Allow for "similar" colors
    */
    
    attrs->closeness = TheFacesResources.closeness;
#else
    attrs->valuemask = XpmReturnPixels | XpmColormap;
#endif
    
    attrs->colormap = colormap;
    
    if (TheXpmResources.override_none_color || shaped)
    {
	csym = XtNew(XpmColorSymbol);
	csym->name  = "None";
	csym->value = TheXpmResources.none_color;
	csym->pixel = 0;
	attrs->colorsymbols = csym;
	attrs->numsymbols = 1;
	attrs->valuemask |= XpmColorSymbols;
    }
    
    return attrs;
}

/*
 *    Read an xpm file.
*/

void *
FaceImageXpmRead(file, type_data)
String	file;
void*	type_data;
{
    Pixmap		pixmap;
    XpmAttributes	*attrs;
    FaceImageXpm	*fip;
    
    /*
     *    Create the attribute struct and throw in the colormap to use.
    */
    
    attrs = FaceImageXpmAttributes(False);
    
    /*
     *    read the file.
    */
    
    if (XpmReadFileToPixmap(display, root, file, &pixmap,
			    NULL, attrs) != XpmSuccess)
    {
	/*
	 *  Oops, failed for some reason.
	*/
	
	XpmFreeAttributes(attrs);
	return(NULL);
    }
    
    fip = (FaceImageXpm *)XtCalloc(1, sizeof(FaceImageXpm));
    
    fip->pixmap     = pixmap;
    fip->shape	    = None;
    fip->attributes = attrs;
    
    return((void *)fip);
}

#ifdef SHAPE
/*
 *    Read a shaped xpm file.
*/

void *
FaceImageShapedXpmRead(file, type_data)
String	file;
void*	type_data;
{
    Pixmap		pixmap;
    Pixmap		shape;
    XpmAttributes	*attrs;
    FaceImageXpm	*fip;
    
    /*
     *    Create the attribute struct and throw in the colormap to use.
    */
    
    attrs = FaceImageXpmAttributes(TheFacesResources.use_shape);
    
    /*
     *    read the file.
    */
    
    if (XpmReadFileToPixmap(display, root, file, &pixmap,
			    &shape, attrs) != XpmSuccess)
    {
	/*
	 *  Oops, failed for some reason.
	*/
	
	XpmFreeAttributes(attrs);
	return(NULL);
    }
    
    fip = (FaceImageXpm *)XtCalloc(1, sizeof(FaceImageXpm));
    
    fip->pixmap     = pixmap;
    fip->shape	    = shape;
    fip->attributes = attrs;
    
    return((void *)fip);
}
#endif

/*
 *    Read an xpm image in via an external filter.
*/

#define	COMMAND_SIZE	1024
#define	BUFFER_SIZE	4096

void *
FaceImageXpmReadFilter(file, type_data)
String        file;
void* type_data;
{
    Pixmap		pixmap;
    XpmAttributes	*attrs;
    FaceImageXpm	*fip;
    char*		filter = type_data;
    FILE*		fp;
    int			len;
    int			c;
    static char*	command;
    static int		command_size;
    static char*	buffer;
    static int		buffer_size;
    
    if (command_size == 0)
    {
	command_size = COMMAND_SIZE;
	command = XtMalloc(command_size);
    }
    
    if (buffer_size == 0)
    {
	buffer_size = BUFFER_SIZE;
	buffer  = XtMalloc(buffer_size);
    }
    
    len = strlen(filter) + strlen(file) + 20;

    if (len >= command_size)
    {
	XtFree(command);
	command_size += len;
	command = XtMalloc(command_size);
    }
    
    sprintf(command, filter, file);
    
    fp = popen(command, "r");
    
    if (fp == NULL)
    {
	return NULL;
    }

    *buffer = '\0';
    
    for(len = 0; (c = fgetc(fp)) != EOF; ++len)
    {
	if (len >= buffer_size)
	{
	    buffer_size += BUFFER_SIZE;
	    buffer = XtRealloc(buffer, buffer_size);
	}
	
	buffer[len]   = c;
	buffer[len+1] = '\0';
    }
    
    len = pclose(fp);

    if (len)
    {
	return NULL;
    }

    attrs = FaceImageXpmAttributes(False);
    
    /*
     *    read the file.
    */
    
    if (XpmCreatePixmapFromBuffer(XtDisplay(TheFrame),
				  DefaultRootWindow(XtDisplay(TheFrame)),
				  buffer, &pixmap,
				  NULL, attrs) != XpmSuccess)
    {
	/*
	 *  Oops, failed for some reason.
	*/
	
	XpmFreeAttributes(attrs);
	return(NULL);
    }
    
    fip = (FaceImageXpm *)XtCalloc(1, sizeof(FaceImageXpm));
    
    fip->pixmap     = pixmap;
    fip->shape	    = None;
    fip->attributes = attrs;
    
    return((void *)fip);
}

/*
 * XFreeColors() causes xfaces to exit with an XLib error on SIEMENS-NIXDORF
 * X11R4 monochrome servers.
 * This catches these errors and ignores them, handling other errors normally.
 * This trick was copied from ghostscript 2.6.1, by Martin.Kraemer@mch.sni.de
*/

static XErrorHandler oldhandler;

static int
x_catch_free_colors(dpy, err)
Display		*dpy;
XErrorEvent	*err;
{
    if (err->request_code == X_FreeColors)
    {
	return 0;
    }
    
    return oldhandler(dpy, err);
}

/*
 *    Free an xpm image.
*/

void
FaceImageXpmFree(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXpm	*fip = data;
    
    /*
     *    Free pixmap.
    */
    
    if (fip->pixmap != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fip->pixmap);
    }
    
    /*
     *    Free shape.
    */
    
    if (fip->shape != None)
    {
	XFreePixmap(XtDisplay(TheFrame), fip->shape);
    }
    
    /*
     *    Free attributes.
    */
    
    if (fip->attributes != NULL)
    {
	/*
	 * Avoid any bogus X error from broken X servers. For instance
	 * the SIEMENS-NIXDORF X11R4 monochrome servers.
	*/
	
	oldhandler = XSetErrorHandler(x_catch_free_colors);
	
	XFreeColors(XtDisplay(TheFrame), fip->attributes->colormap,
		    fip->attributes->pixels, fip->attributes->npixels, 0);
	
	/*
	 * Force any errors to happen here so we can catch them!
	*/
	
	XSync (XtDisplay(TheFrame), False);
	oldhandler = XSetErrorHandler(oldhandler);
	
	XpmFreeAttributes(fip->attributes);
	XtFree((void *)(fip->attributes));
    }
    
    XtFree((void *)fip);
}

/*
 *    Return pixmap for xpm file.
*/

Pixmap
FaceImageXpmPixmap(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXpm	*fip = data;
    return(fip->pixmap);
}

/*
 *    Return shape pixmap for xpm file.
*/

Pixmap
FaceImageXpmShape(data, type_data)
void	*data;
void*	type_data;
{
    FaceImageXpm	*fip = data;
    return(fip->shape);
}


static FaceImageType FaceImageTypeXpm = 
{
    "xpm",
    FaceImageXpmRead,
    FaceImageXpmFree,
    FaceImageXpmPixmap,
    FaceImageXpmShape,
    ".xpm",
    NULL,
    NULL
};

#ifdef SHAPE
static FaceImageType FaceImageTypeShapedXpm = 
{
    "xpm-shape",
    FaceImageShapedXpmRead,
    FaceImageXpmFree,
    FaceImageXpmPixmap,
    FaceImageXpmShape,
    "-shape.xpm",
    NULL,
    NULL
};
#endif

void
FaceImageXpmInit()
{
    int			i;
    FaceXpmFilter	filter;
    char		name[50];
    FaceImageType*	type;
    
    FaceImageTypeRegister(&FaceImageTypeXpm);
#ifdef SHAPE
    FaceImageTypeRegister(&FaceImageTypeShapedXpm);
#endif

    /*
     * Cache some values.
    */
    
    display = XtDisplay(TheTopLevel);
    root    = DefaultRootWindow(display);
    XtVaGetValues(TheTopLevel, XtNcolormap, &colormap, NULL);

    /*
     * Get the main xpm resources.
    */
    
    XtGetSubresources(TheTopLevel, &TheXpmResources, "xpm", "Xpm",
		      FaceXpmResourcesList,
		      XtNumber(FaceXpmResourcesList),
		      NULL, 0);

    /*
     *  Now create any filters.
    */
    
    for(i = 0; i < TheXpmResources.filter_count; ++i)
    {
	sprintf(name, "xpmFilter%d", i+1);
	
	filter.name      = NULL;
	filter.filter  	= NULL;
	filter.extension = NULL;
	
	XtGetSubresources(TheTopLevel, &filter, name, "XpmFilter",
			  FaceXpmFilterResourcesList,
			  XtNumber(FaceXpmFilterResourcesList),
			  NULL, 0);
	
	if (filter.name      && *filter.name      != '\0' &&
	    filter.filter    && *filter.filter    != '\0' &&
	    filter.extension && *filter.extension != '\0')
	{
	    type = XtNew(FaceImageType);
	    
	    type->name      = filter.name;
	    type->read      = FaceImageXpmReadFilter;
	    type->free      = FaceImageXpmFree;
	    type->pixmap    = FaceImageXpmPixmap;
	    type->shape     = FaceImageXpmShape;
	    type->extension = filter.extension;
	    type->data      = filter.filter;
	    
	    FaceImageTypeRegister(type);
	}
    }
}
