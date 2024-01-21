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
#ifndef ToggleP_h
#define ToggleP_h

#include "Toggle.h"
#include "KnappP.h"

typedef struct {
    XtPointer	empty;
} ToggleClassPart;

typedef struct ToggleClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    KnappClassPart	knapp_class;
    ToggleClassPart	toggle_class;
} ToggleClassRec;

extern ToggleClassRec toggleClassRec;

typedef struct {
    Dimension	toggle_size;
    Dimension	toggle_offset;
    Dimension	toggle_shadow_width;
    /* private data */
    Boolean	set;
} TogglePart;

typedef struct ToggleRec {
    CorePart		core;
    ShadowPart		shadow;
    KnappPart		knapp;
    TogglePart		toggle;
} ToggleRec;

#endif /* ToggleP_h */
