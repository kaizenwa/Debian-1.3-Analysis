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
#ifndef KnappP_h
#define KnappP_h

#include "Knapp.h"
#include "Util.h"
#include "ShadowP.h"

typedef struct {
    XtPointer	empty;
} KnappClassPart;

typedef struct KnappClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    KnappClassPart	knapp_class;
} KnappClassRec;

extern KnappClassRec knappClassRec;

typedef struct {
    Pixel		foreground_pixel;
    XFontStruct		*font;
    String		label;
    XtCallbackList      callback;
    Dimension		internal_height;
    Dimension		left_margin;
    Dimension		right_margin;
    JustifyType		justify;
    Boolean		resizable;
    /* private data */
    GC			default_gc;
    GC			gray_gc;
    Pixmap		stipple;
    char		**labels;
    short		no_labels;
    short		label_x;
    short		label_width;
    short		max_width;
    short		label_no;
    Boolean		set;
    Boolean		calling_callbacks;
    Boolean		active;
} KnappPart;

typedef struct KnappRec {
    CorePart	core;
    ShadowPart	shadow;
    KnappPart	knapp;
} KnappRec;

#endif /* KnappP_h */

