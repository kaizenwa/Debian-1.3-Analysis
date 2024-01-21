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
#ifndef ScrollableP_h
#define ScrollableP_h

#include "Scrollable.h"
#include "ScrBar.h"
#include "ShadowP.h"

typedef void	(*ScrollableSetPosProc)(ScrollableWidget, long);
typedef void	(*ScrollableSuspendHook)(ScrollableWidget);

#define XtInheritScrollableSetPos	((ScrollableSetPosProc)_XtInherit)
#define XtInheritScrollableSuspendHook	((ScrollableSuspendHook)_XtInherit)

typedef struct {
    ScrollableSetPosProc	set_hpos;
    ScrollableSetPosProc	set_vpos;
    ScrollableSuspendHook	suspend_hook;
    XtPointer			extension;
} ScrollableClassPart;

typedef struct ScrollableClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    ScrollableClassPart	scrollable_class;
} ScrollableClassRec;

extern ScrollableClassRec	scrollableClassRec;

typedef struct {
    Widget		h_bar;
    Widget		v_bar;
    /* private */
    long		pos_x;
    long		shown_x;
    long		width;
    long		pos_y;
    long		shown_y;
    long		height;
    int			suspended;
} ScrollablePart;

typedef struct ScrollableRec {
    CorePart		core;
    ShadowPart		shadow;
    ScrollablePart	scrollable;
} ScrollableRec;

extern void	ScrollableFitHBar(ScrollableWidget);
extern void	ScrollableFitVBar(ScrollableWidget);
extern void	ScrollableHFromGeometry(ScrollableWidget);
extern void	ScrollableVFromGeometry(ScrollableWidget);

#endif /* ScrollableP_h */
