/**
 *
 * $Id: ImageCache.c,v 1.7 1996/12/17 03:24:00 miers Exp $
 *
 * Copyright (C) 1995 Free Software Foundation, Inc.
 *
 * This file is part of the GNU LessTif Library.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/

static char rcsid[] = "$Id: ImageCache.c,v 1.7 1996/12/17 03:24:00 miers Exp $";

/* Some stuff unimplemented as of 30/8/1996 */
#undef	HAVE_FIXED_IMAGE_CACHE

#include <LTconfig.h>
#include <XmI/XmI.h>
#include <XmI/ImageCacheI.h>

#include <Xm/XmP.h>
#include <Xm/ScreenP.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <XmI/DebugUtil.h>

/* the actual caches for images and for pixmaps */

static XmImageCache image_cache;
static XmPixmapCache pixmap_cache;

/* internal functions */
static XImage *__XmGetImageFromCache(char *image_name);
static void __XmAddImageToCache(char *image_name, XImage *image);
static void __XmRemoveImageFromCache(int index);

static Pixmap __XmGetPixmapFromCache(char *pixmap_name, Screen *screen, Pixel foreground, Pixel background, int depth);
void __XmAddPixmapToCache(char *pixmap_name, Pixmap pixmap, Screen *screen, Pixel foreground, Pixel background, int depth);
static void __XmRemovePixmapFromCache(int index);

static void __XmCreateSearchPath(void);

/* Various paths used when searching for pixmaps in XmGetPixmap below */
static char *_search_path = NULL;

static char *XAPPLRESDIR_set_pattern = "\
%%B:\
%s/%%L/%%T/%%N/%%B:\
%s/%%l/%%T/%%N/%%B:\
%s/%%T/%%N/%%B:\
%s/%%L/%%T/%%B:\
%s/%%l/%%T/%%B:\
%s/%%T/%%B:\
%s/%%T/%%B:\
%s/%%B:\
/usr/lib/X11/%%L/%%T/%%N/%%B:\
/usr/lib/X11/%%l/%%T/%%N/%%B:\
/usr/lib/X11/%%T/%%N/%%B:\
/usr/lib/X11/%%L/%%T/%%B:\
/usr/lib/X11/%%l/%%T/%%B:\
/usr/lib/X11/%%T/%%B:\
/usr/include/X11/%%T/%%B";

static char *nothing_set_pattern = "\
%%B:\
%s/%%L/%%T/%%N/%%B:\
%s/%%l/%%T/%%N/%%B:\
%s/%%T/%%N/%%B:\
%s/%%L/%%T/%%B:\
%s/%%l/%%T/%%B:\
%s/%%T/%%B:\
%s/%%B:\
/usr/lib/X11/%%L/%%T/%%N/%%B:\
/usr/lib/X11/%%l/%%T/%%N/%%B:\
/usr/lib/X11/%%T/%%N/%%B:\
/usr/lib/X11/%%L/%%T/%%B:\
/usr/lib/X11/%%l/%%T/%%B:\
/usr/lib/X11/%%T/%%B:\
/usr/include/X11/%%T/%%B";

Pixmap
XmGetPixmap(Screen *screen,
	    char *image_name,
	    Pixel foreground,
	    Pixel background)
{
	Pixmap	p;
	p = XmGetPixmapByDepth(screen, image_name, foreground, background,
			      DefaultDepthOfScreen(screen));
	XdbDebug(__FILE__, NULL, "XmGetPixmap(%s, %d, %d) => 0x%x (depth %d)\n",
		image_name, foreground, background, p,
		DefaultDepthOfScreen(screen));
	return p;
}

Pixmap
XmGetPixmapByDepth(Screen *screen,
		   char *image_name,
		   Pixel foreground,
		   Pixel background,
		   int depth)
{
    char *pathname_to_pixmap;
    unsigned int bitmap_width, bitmap_height;
    int x_hot, y_hot;
    XImage *image;
    Pixmap new_pix, tmp;
    GC gc;
    XGCValues values;

    values.foreground = foreground;
    values.background = background;

    /* first, we check the pixmap cache to see if it's there. */
    new_pix = __XmGetPixmapFromCache(image_name, screen, foreground, background, depth);

    if (new_pix != XmUNSPECIFIED_PIXMAP) /* a match from the pixmap cache */
	return new_pix;

    /* ok, check the image cache */
    image = __XmGetImageFromCache(image_name);

    if (image != NULL) /* a match from the image cache */
    {
	/* create the pixmap, add it to the cache, and return it. */

	int image_depth;

	if (image->format == XYBitmap)
	    image_depth = 1;
	else
	    image_depth = image->depth;

	/* create the pixmap */
	new_pix = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
					depth, image->width, image->height);

	tmp = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
				    depth, image->width, image->height);
			   
	gc = XCreateGC(DisplayOfScreen(screen), tmp, 0, NULL);

	/* move the image information into a temporary pixmap */
	XPutImage(DisplayOfScreen(screen),
		  tmp,
		  gc,
		  image,
		  0,0,
		  0,0,
		  image->width, image->height);

	XFreeGC(DisplayOfScreen(screen), gc);

	values.foreground = background;
	values.background = foreground;

	gc = XCreateGC(DisplayOfScreen(screen), new_pix, GCForeground|GCBackground, &values);

	if (image_depth == 1)
	{
	    XCopyPlane(DisplayOfScreen(screen), 
		       tmp, 
		       new_pix,
		       gc, 
		       0,0, 
		       image->width, image->height,
		       0,0, 
		       1);
	}
	else
	{
	    XCopyArea(DisplayOfScreen(screen),
		      tmp,
		      new_pix,
		      gc,
		      0, 0,
		      image->width, image->height,
		      0, 0);
	}

	XFreeGC(DisplayOfScreen(screen), gc);
	
	/* add the pixmap to the cache */
	__XmAddPixmapToCache(image_name,
			     new_pix,
			     screen,
			     foreground,
			     background,
			     depth);

	/* and return it */
	return new_pix;
    }

    /* since it wasn't in either cache, we search for the file */
    /* make sure the search path is there */
    if (!_search_path)
      __XmCreateSearchPath();

    if (image_name[0] == '/') /* an absolute pathname */
	pathname_to_pixmap = XtNewString(image_name);
    else
    {
	SubstitutionRec subs[1];

	subs[0].match = 'B';
	subs[0].substitution = XtNewString(image_name);

	pathname_to_pixmap = XtResolvePathname(DisplayOfScreen(screen),
					       "bitmaps",
					       NULL,
					       NULL,
					       _search_path,
					       subs,
					       1,
					       NULL);
        XtFree(subs[0].substitution);
    }					 

    /* this breaks if X isn't where it's supposed to be. Don't dump core,
     * just return */
    if (pathname_to_pixmap == NULL || strlen(pathname_to_pixmap) == 0)
       return XmUNSPECIFIED_PIXMAP;

    XdbDebug(__FILE__, NULL, "pathname found is %s\n", pathname_to_pixmap);
    
    if (XReadBitmapFile(DisplayOfScreen(screen),
			RootWindowOfScreen(screen),
			pathname_to_pixmap,
			&bitmap_width, &bitmap_height,
			&tmp, &x_hot, &y_hot) != BitmapSuccess)
	_XmWarning (NULL, "Couldn't load the pixmap %s.\n", pathname_to_pixmap);

    XtFree(pathname_to_pixmap);

    new_pix = _XmAllocScratchPixmap((XmScreen)XmGetXmScreen(screen),
				    depth, bitmap_width,bitmap_height);

    gc = XCreateGC(DisplayOfScreen(screen), new_pix, GCForeground|GCBackground, &values);

    XCopyPlane(DisplayOfScreen(screen), 
	       tmp, 
	       new_pix,
	       gc, 
	       0,0, 
	       bitmap_width, bitmap_height,
	       0,0, 
	       1);

    XFreeGC(DisplayOfScreen(screen), gc);

    /* add the pixmap to the cache */
    __XmAddPixmapToCache(image_name,
			 new_pix,
			 screen,
			 foreground,
			 background,
			 depth);

    return new_pix;
}

Boolean
XmDestroyPixmap(Screen *screen,
		Pixmap pixmap)
{
    int i;

    for (i=0; i < pixmap_cache->number_of_entries; i++)
	if (pixmap_cache->entries[i].pixmap == pixmap
	    && pixmap_cache->entries[i].screen == screen)
	{
	    pixmap_cache->entries[i].ref_count --;

	    if (pixmap_cache->entries[i].ref_count == 0)
		__XmRemovePixmapFromCache(i);
	    
	    return True;
	}

    return False;
}

Boolean
XmInstallImage(XImage *image,
	       char *image_name)
{
    if (image == NULL ||
	image_name == NULL)
	return False;

    if (image_cache == NULL)
	_XmSetupImageCache();

    if (__XmGetImageFromCache(image_name) != NULL) /* the image already exists */
	return False;

    __XmAddImageToCache(image_name, image);

    return True; /* FIX ME */
}

Boolean
XmUninstallImage(XImage *image)
{
    int i;

    if (image == NULL)
	return False;
    
    if (image_cache == NULL)
	_XmSetupImageCache();

    for (i=0; i < image_cache->number_of_entries; i++)
	if (image_cache->entries[i].image == image)
	{
	    __XmRemoveImageFromCache(i);
	    return True;
	}

    return False;
}

/* private LessTif functions */

/* initialize the image and pixmap caches. */
void
_XmSetupImageCache()
{
    image_cache = (XmImageCache)XtMalloc(sizeof(XmImageCacheRec));
    pixmap_cache = (XmPixmapCache)XtMalloc(sizeof(XmPixmapCacheRec));

    /* start them off rather small */
    image_cache->max_number_of_entries = 
	pixmap_cache->max_number_of_entries = 10;
    
    image_cache->number_of_entries = 
	pixmap_cache->number_of_entries = 0;

    image_cache->entries = (XmImageCacheEntry)XtCalloc(image_cache->max_number_of_entries,
						       sizeof(XmImageCacheEntryRec));
    pixmap_cache->entries = (XmPixmapCacheEntry)XtCalloc(pixmap_cache->max_number_of_entries,
							 sizeof(XmPixmapCacheEntryRec));
}

/* retrieve an image from the cache, given its name */
static XImage *
__XmGetImageFromCache(char *image_name)
{
    int i;

    if (image_cache == NULL)
	_XmSetupImageCache();

    for (i=0; i<image_cache->number_of_entries; i++)
	if (!strcmp(image_name, image_cache->entries[i].image_name))
	    return image_cache->entries[i].image;
    
    return NULL;
}

/* add an image to the cache, resizing it as necessary */
static void 
__XmAddImageToCache(char *image_name, 
		    XImage *image)
{
    if (image_cache == NULL)
	_XmSetupImageCache();

    if (image_cache->number_of_entries == image_cache->max_number_of_entries)
    {
	image_cache->max_number_of_entries += 5; /* maybe we should double the size? */

	image_cache->entries = (XmImageCacheEntry)XtRealloc((char*)image_cache->entries, 
							    (image_cache->max_number_of_entries 
							     * sizeof(XmImageCacheEntryRec)));
    }

    image_cache->entries[image_cache->number_of_entries].image = image;
    image_cache->entries[image_cache->number_of_entries].image_name = XtNewString(image_name);

    image_cache->number_of_entries++;
}

static void 
__XmRemoveImageFromCache(int index)
{
    /* we don't free the image, we just slide everything
       down in the cache */

    int j;

    if (image_cache == NULL)
	_XmSetupImageCache();

    for (j=index; j<image_cache->number_of_entries-1; j++)
	image_cache->entries[j] = image_cache->entries[j+1];
    
    image_cache->number_of_entries--;
}

/* retrieve a pixmap from the pixmap cache, given its name */
static Pixmap 
__XmGetPixmapFromCache(char *pixmap_name, 
		       Screen *screen, 
		       Pixel foreground, 
		       Pixel background, 
		       int depth)
{
    int i;

    for (i=0; i<pixmap_cache->number_of_entries; i++)
	if (!strcmp(pixmap_name, pixmap_cache->entries[i].pixmap_name)
	    && screen == pixmap_cache->entries[i].screen 
	    && foreground == pixmap_cache->entries[i].foreground
	    && background == pixmap_cache->entries[i].background
	    && depth == pixmap_cache->entries[i].depth)
	{
	    /* increment the reference count and return the pixmap */
	    
	    pixmap_cache->entries[i].ref_count ++;

	    return pixmap_cache->entries[i].pixmap;
	}
    
    return XmUNSPECIFIED_PIXMAP;
}

/* add a pixmap to the pixmap cache, possibly reallocating it */
void 
__XmAddPixmapToCache(char *pixmap_name, 
		     Pixmap pixmap,
		     Screen *screen, 
		     Pixel foreground, 
		     Pixel background,
		     int depth)
{
    if (pixmap_cache->number_of_entries == pixmap_cache->max_number_of_entries)
    {
	pixmap_cache->max_number_of_entries += 5; /* maybe we should double the size? */

	pixmap_cache->entries = (XmPixmapCacheEntry)XtRealloc((char*)pixmap_cache->entries, 
							      (pixmap_cache->max_number_of_entries 
							       * sizeof(XmPixmapCacheEntryRec)));
    }

    pixmap_cache->entries[pixmap_cache->number_of_entries].pixmap = pixmap;
    pixmap_cache->entries[pixmap_cache->number_of_entries].pixmap_name = XtNewString(pixmap_name);
    pixmap_cache->entries[pixmap_cache->number_of_entries].screen = screen;
    pixmap_cache->entries[pixmap_cache->number_of_entries].foreground = foreground;
    pixmap_cache->entries[pixmap_cache->number_of_entries].background = background;
    pixmap_cache->entries[pixmap_cache->number_of_entries].depth = depth;

    /* initialize the reference count for this pixmap */
    pixmap_cache->entries[pixmap_cache->number_of_entries].ref_count = 1;

    pixmap_cache->number_of_entries++;
}

static void 
__XmRemovePixmapFromCache(int index)
{
    /* we don't free the pixmap, we just slide everything
       down in the cache */

    int j;

    for (j=index; j<pixmap_cache->number_of_entries-1; j++)
	pixmap_cache->entries[j] = pixmap_cache->entries[j+1];
    
    pixmap_cache->number_of_entries--;
}

static void
__XmCreateSearchPath()
{
    char *XBMLANGPATH = XtNewString((String)getenv("XBMLANGPATH"));
    char *XAPPLRESDIR = XtNewString((String)getenv("XAPPLRESDIR"));

    if (XBMLANGPATH)
    {
	_search_path = XBMLANGPATH;

	if (XAPPLRESDIR)
	    XtFree(XAPPLRESDIR);
    }
    else if (XAPPLRESDIR)
    {
	char *HOME=getenv("HOME");
	int HOME_length = strlen(HOME);
	int XAPPLRESDIR_length = strlen(XAPPLRESDIR);

	_search_path = (char*)XtMalloc(strlen(XAPPLRESDIR_set_pattern)
				       + XAPPLRESDIR_length * 6
				       + HOME_length * 2 + 1);

	sprintf (_search_path, XAPPLRESDIR_set_pattern,
		 XAPPLRESDIR,XAPPLRESDIR,XAPPLRESDIR,
		 XAPPLRESDIR,XAPPLRESDIR,XAPPLRESDIR,
		 HOME,HOME);
		 

    }
    else /* neither of them was set... */
    {
	char *HOME=getenv("HOME");
	int HOME_length = strlen(HOME);

	_search_path = (char*)XtMalloc(strlen(nothing_set_pattern)
				       + HOME_length * 7 + 1);

	sprintf (_search_path, nothing_set_pattern,
		 HOME,HOME,HOME,HOME,HOME,HOME,HOME);
    }

    XdbDebug(__FILE__, NULL, "Using %s for search path\n", _search_path);
}

/*
 * Found description of this sucker in McMinds & Whitty, "Writing Your Own OSF/Motif Widgets"
 * HP Professional Books, ISBN 0-13-104191-6, kindly donated to the LessTif project by
 * Linux International.
 */
/*
 * Find pixmap by (screen, pixmap); return True if it's in the cache.
 */
Boolean
_XmGetPixmapData(Screen *screen,
		Pixmap pixmap,
		char **image_name,
		int *depth,
		Pixel *foreground,
		Pixel *background,
		int *hot_x,
		int *hot_y,
		unsigned int *width,
		unsigned int *height)
{
	int	i;

	for (i=0; i<pixmap_cache->number_of_entries; i++)
	    if (screen == pixmap_cache->entries[i].screen 
			&& pixmap == pixmap_cache->entries[i].pixmap) {
		    if (image_name)
			*image_name = XtNewString(pixmap_cache->entries[i].pixmap_name);
		    if (depth)
			*depth = pixmap_cache->entries[i].depth;
		    if (foreground)
			*foreground = pixmap_cache->entries[i].foreground;
		    if (background)
			*background = pixmap_cache->entries[i].background;
/* FIX ME - don't have these in LessTif ImageCache yet */
#if HAVE_FIXED_IMAGE_CACHE
		    if (hot_x)
			*hot_x = pixmap_cache->entries[i].hot_x;
		    if (hot_y)
			*hot_y = pixmap_cache->entries[i].hot_y;
#endif
/* FIX ME - these could be obtained with Xlib (XGetGeometry) but that seems so silly */
#if HAVE_FIXED_IMAGE_CACHE
		    if (width)
			*width = pixmap_cache->entries[i].width;
		    if (height)
			*height = pixmap_cache->entries[i].height;
#endif
		    return True;
		}
	return False;
}

/* Motif 2.* version of the above */
Boolean
XmeGetPixmapData(Screen *screen,
		Pixmap pixmap,
		char **image_name,
		int *depth,
		Pixel *foreground,
		Pixel *background,
		int *hot_x,
		int *hot_y,
		unsigned int *width,
		unsigned int *height)
{
	return _XmGetPixmapData(screen, pixmap, image_name, depth, foreground, background,
		hot_x, hot_y, width, height);
}

#ifdef NONSTANDARD_EXTENSIONS
/*
 * _XmeGetPixmap is not part of the OSF/Motif API.
 * This routine is used with the string to pixmap converter
 * routine.  It does a lookup into the cache based on the name
 * of the pixmap and also where the foreground, background
 * and depth are equal to zero.
 */
#include<XmI/XmXpm.h>
Pixmap
_XmeGetPixmap(Screen *screen, char *fname)
{
   static Colormap _cmap;
 
   XmXpmAttributes xpm_attrib;
   Pixmap pmap;
   Pixmap mask;

   Display *dpy;
   Window w;
 
   char *pathname_to_pixmap = NULL;

   dpy = DisplayOfScreen(screen);
   w = RootWindowOfScreen(screen);
 
   /* initialize colormap if it hasn't been done */
   if(_cmap == (Colormap) NULL)
   {
      XWindowAttributes w_attrib;
      XGetWindowAttributes(dpy,w,&w_attrib);
      _cmap=w_attrib.colormap;
   }
 
   /*
    * Set the foreground, background and depth all to zero 
    * for now.
    */
   pmap = __XmGetPixmapFromCache(fname,screen,0,0,0);
   if (pmap != XmUNSPECIFIED_PIXMAP)
   {
      return pmap;
   }
 
   if(_search_path == NULL)
   {
      __XmCreateSearchPath();
   }
 
   /*
    * Attempt to find pixmap in search_path
    * if an absolute path was not given.
    */
   if(fname != NULL && fname[0]=='/')
   {
      pathname_to_pixmap = XtNewString(fname);
   }
   else
   {
      SubstitutionRec sub;
 
      sub.match = 'B';
      sub.substitution = fname;
 
      pathname_to_pixmap = XtResolvePathname(dpy,
					     "bitmaps",
                                             NULL,
                                             NULL,
                                             _search_path,
                                             &sub,
                                             1,
                                             NULL);
   }
 
   if (pathname_to_pixmap == NULL
       || strlen(pathname_to_pixmap) == 0)
   {
      return XmUNSPECIFIED_PIXMAP;
   }
 
   xpm_attrib.colormap=_cmap;
   xpm_attrib.closeness=40000;
   xpm_attrib.valuemask=XmXpmSize | XmXpmReturnPixels
                        | XmXpmColormap | XmXpmCloseness;
 
   if(_XmXpmReadFileToPixmap(dpy,w,pathname_to_pixmap,&pmap,&mask,&xpm_attrib)
      == XmXpmSuccess)
   {
      __XmAddPixmapToCache(fname,pmap,screen,0,0,0);
   }
   else
   {
      /* could not find it so lets return it as unspecified */
      pmap = XmUNSPECIFIED_PIXMAP;
   }
 
   XtFree(pathname_to_pixmap);

   return pmap;
}
#endif
