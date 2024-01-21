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
#ifndef MenuKnappP_h
#define MenuKnappP_h

#include "MenuKnapp.h"
#include "KnappP.h"

typedef struct {
    XtPointer	empty;
} MenuKnappClassPart;

typedef struct MenuKnappClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    KnappClassPart	knapp_class;
    MenuKnappClassPart	menu_knapp_class;
} MenuKnappClassRec;

extern MenuKnappClassRec menuKnappClassRec;

enum {
    MenuStateDown,
    MenuStateUp,
    MenuStateWaiting
};

typedef struct {
    String		menu_name;
    int			multi_click_time;
    int			popdown_time;
    Dimension		arrow_size;
    Dimension		arrow_offset;
    Dimension		arrow_shadow_width;
    /* private data */
    Widget		menu;
    Time		start_time;
    int			menu_state;
    XtIntervalId	timer;
} MenuKnappPart;

typedef struct MenuKnappRec {
    CorePart		core;
    ShadowPart		shadow;
    KnappPart		knapp;
    MenuKnappPart	menu_knapp;
} MenuKnappRec;

#endif /* MenuKnappP_h */
