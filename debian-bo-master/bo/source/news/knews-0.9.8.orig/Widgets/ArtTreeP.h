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
#ifndef ArtTreeP_h
#define ArtTreeP_h

#include "ArtTree.h"
#include "ArtTreeNode.h"
#include "ScrollableP.h"

typedef struct {
    XtPointer	extension;
} ArtTreeClassPart;

typedef struct ArtTreeClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    ScrollableClassPart	scrollable_class;
    ArtTreeClassPart	arttree_class;
} ArtTreeClassRec;

extern ArtTreeClassRec artTreeClassRec;

typedef struct {
    Pixel               foreground_pixel;
    Pixel		inner_pixel;
    Pixel		outer_pixel;
    Pixel		rubber_pixel;
    Cursor              cursor;
    XFontStruct		*font;
    Dimension		row_spacing;
    Dimension		column_spacing;
    Dimension		internal_width;
    Dimension		internal_height;
    Dimension		internal_node_width;
    Dimension		internal_node_height;
    Dimension		node_columns;
    Dimension		node_rows;
    Dimension		pixmap_width;
    Dimension		pixmap_height;
    Dimension		pixmap_spacing;
    Boolean		inner_dashed;
    Boolean		outer_dashed;
    Boolean		depth_one;
    Boolean		warp_pointer;
    Boolean		vertical;
    XtCallbackList	inner_callback;
    XtCallbackList	outer_callback;
    XtCallbackList	select_callback;
    XtCallbackList	callback;
    ART_TREE_NODE	*root;
    /* private state */
    GC			default_gc;
    GC			inner_gc;
    GC			outer_gc;
    GC			bg_gc;
    GC			light_dashed_gc;
    GC			dark_dashed_gc;
    GC			rubber_gc;
    short		node_width;
    short		node_height;
    short		ptr_init_x;
    short		ptr_init_y;
    short		init_x;
    short		init_y;
    Boolean		rubberbanding;
    Boolean		active;
} ArtTreePart;

typedef struct ArtTreeRec {
    CorePart		core;
    ShadowPart		shadow;
    ScrollablePart	scrollable;
    ArtTreePart		arttree;
} ArtTreeRec;

typedef struct art_tree_private {
    Pixmap		pixmap;
    int			label_len;
    short		x;
    short		y;
    short		bb_width;
    short		bb_height;
    Boolean		selected;
    Boolean		dashed;
    Boolean		inner;
    Boolean		outer;
} ART_TREE_PRIVATE;

#endif /* ArtTreeP_h */
