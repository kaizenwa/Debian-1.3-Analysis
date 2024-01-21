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
#ifndef ScrlistP_h
#define ScrlistP_h

#include "ScrList.h"
#include "ScrollableP.h"

typedef struct {
    XtPointer	extension;
} ScrListClassPart;

typedef struct ScrListClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    ScrollableClassPart	scrollable_class;
    ScrListClassPart	scrlist_class;
} ScrListClassRec;

extern ScrListClassRec	scrListClassRec;

typedef struct {
    Pixel		foreground_pixel;
    Pixel		highlight_pixel;
    Cursor		dnd_cursor;
    XFontStruct		*font;
    XtCallbackList	select_callback;
    XtCallbackList	callback;
    XtCallbackList	second_callback;
    XtCallbackList	dnd_callback;
    long		n_alloc;
    Dimension		row_spacing;
    Dimension		internal_width;
    Dimension		internal_height;
    Dimension		internal_item_width;
    Dimension		internal_item_height;
    Dimension		pixmap_width;
    Dimension		pixmap_height;
    Dimension		pixmap_spacing;
    Dimension		preferred_lines;
    Dimension		preferred_columns;
    Dimension		margin_up;
    Dimension		margin_down;
    Boolean		depth_one;
    Boolean		at_least_one;
    Boolean		at_most_one;
    Boolean		allow_dnd;
    Boolean		use_pixmaps;
    Boolean		page_up;
    Boolean		page_down;
    /* private */
    Boolean		active;
    char		**strings;
    Pixmap		*pixmaps;
    Boolean		*selected;
    long		n_sel;
    long		dnd_start;
    GC			default_gc;
    GC			selected_gc;
    GC			highlight_gc;
    int			max_width;
} ScrListPart;

typedef struct ScrListRec {
    CorePart		core;
    ShadowPart		shadow;
    ScrollablePart	scrollable;
    ScrListPart		scrlist;
} ScrListRec;

#endif /* ScrListP_h */
