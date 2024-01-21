/**
 *
 * $Id: ImageCacheI.h,v 1.2 1996/08/29 00:07:28 miers Exp $
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

#ifndef XM_IMAGECACHE_I
#define XM_IMAGECACHE_I

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _XmPixmapCacheEntryRec {
    Pixmap pixmap;
    char *pixmap_name;
    Screen *screen;
    Pixel foreground;
    Pixel background;
    int depth;

    int ref_count;
} XmPixmapCacheEntryRec, *XmPixmapCacheEntry;

typedef struct _XmPixmapCacheRec {
    XmPixmapCacheEntry entries;
    int number_of_entries;
    int max_number_of_entries;
} XmPixmapCacheRec, *XmPixmapCache;

typedef struct _XmImageCacheEntryRec {
    XImage *image;
    char *image_name;
} XmImageCacheEntryRec, *XmImageCacheEntry;

typedef struct _XmImageCacheRec {
    XmImageCacheEntry entries;
    int number_of_entries;
    int max_number_of_entries;
} XmImageCacheRec, *XmImageCache;

#ifdef __cplusplus
}
#endif

#endif /* XM_IMAGECACHE_I */
