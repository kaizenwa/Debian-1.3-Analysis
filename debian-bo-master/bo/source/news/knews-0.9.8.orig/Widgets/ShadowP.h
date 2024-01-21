/* This software is Copyright 1995, 1996 by Karl-Johan Johnsson
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ANY USE OF THIS
 * SOFTWARE IS AT THE USERS OWN RISK.
 */
#ifndef ShadowP_h
#define ShadowP_h

#include "Shadow.h"
#include <X11/CoreP.h>

typedef Boolean (*ShadowAllocShadowColorsProc)(ShadowWidget, XColor*);
typedef void    (*ShadowAllocShadowPixmapsProc)(ShadowWidget, Pixel);
typedef Boolean (*ShadowAllocArmColorProc)(ShadowWidget, XColor*);
typedef void    (*ShadowAllocArmPixmapProc)(ShadowWidget, Pixel);
typedef void    (*ShadowAllocGCsProc)(ShadowWidget);

#define XtInheritPixelOffset		\
    0 /* this is a bit unortodox... */
#define XtInheritAllocShadowColors	\
    ((ShadowAllocShadowColorsProc)_XtInherit)
#define XtInheritAllocShadowPixmaps	\
    ((ShadowAllocShadowPixmapsProc)_XtInherit)
#define XtInheritAllocArmColor		\
    ((ShadowAllocArmColorProc)_XtInherit)
#define XtInheritAllocArmPixmap		\
    ((ShadowAllocArmPixmapProc)_XtInherit)
#define XtInheritAllocGCs		\
    ((ShadowAllocGCsProc)_XtInherit)

typedef struct {
    Cardinal				pixel_offset;
    Boolean				use_arm_for_background;
    ShadowAllocShadowColorsProc		alloc_shadow_colors;
    ShadowAllocShadowPixmapsProc	alloc_shadow_pixmaps;
    ShadowAllocArmColorProc		alloc_arm_color;
    ShadowAllocArmPixmapProc		alloc_arm_pixmap;
    ShadowAllocGCsProc			alloc_gcs;
    XtPointer				extension;
} ShadowClassPart;

typedef struct ShadowClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
} ShadowClassRec;

extern ShadowClassRec shadowClassRec;

typedef struct {
    Dimension	shadow_width;
    Boolean	alloc_shadow_colors;
    Boolean	use_lines;
    Boolean	alloc_arm_color;
    Boolean	alloc_arm_pixmap;
    /* private data */
    Pixel	light_pixel;
    Pixmap	light_pixmap;
    GC		light_gc;
    Pixel	dark_pixel;
    Pixmap	dark_pixmap;
    GC		dark_gc;
    Pixel	arm_pixel;
    Pixmap	arm_pixmap;
    GC		arm_gc;
    Boolean	line_mode;
    Boolean	alloced_shadow_pixels;
    Boolean	alloced_arm_pixel;
} ShadowPart;

typedef struct ShadowRec {
    CorePart	core;
    ShadowPart	shadow;
} ShadowRec;

extern void ShadowDrawShadows(ShadowWidget, Position, Position,
			      Dimension, Dimension, Boolean);

#endif /* ShadowP_h */
