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
#ifndef ScrBarP_h
#define ScrBarP_h

#include "ScrBar.h"
#include "Util.h"
#include "ShadowP.h"

typedef enum {
    ScrBarModeNone,
    ScrBarModeForwardLine,
    ScrBarModeBackwardLine,
    ScrBarModeForwardPage,
    ScrBarModeBackwardPage,
    ScrBarModeContinuous
} ScrBarMode;

typedef struct {
    XtPointer	extension;
} ScrBarClassPart;

typedef struct ScrBarClassRec {
    CoreClassPart       core_class;
    ShadowClassPart	shadow_class;
    ScrBarClassPart     scrbar_class;
} ScrBarClassRec;

extern ScrBarClassRec scrBarClassRec;

typedef struct {
    long		canvas_length;
    long		slider_length;
    long		slider_position;
    long		step_size;
    XtCallbackList      scroll_callback;
    int			initial_delay;
    int			minimum_delay;
    int			decay;
    Dimension		minimum_thumb;
    Dimension		thickness;
    Boolean		vertical;
    Boolean		push_thumb;
    Boolean		allow_off;
    Boolean		sync_scroll;
    /* private data */
    GC			bg_gc;
    Position		c1, c2, c3, c4;
    Position		init_ptr_pos;
    Position		init_slider_pos;
    ScrBarMode		mode;
    XtIntervalId	timer;
    int			this_delay;
} ScrBarPart;

typedef struct ScrBarRec {
    CorePart    core;
    ShadowPart	shadow;
    ScrBarPart  scrbar;
} ScrBarRec;

#endif /* ScrBarP_h */
