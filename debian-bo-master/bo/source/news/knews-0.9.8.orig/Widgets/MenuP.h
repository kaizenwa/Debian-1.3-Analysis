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
#ifndef MenuP_h
#define MenuP_h

#include "Menu.h"
#include "ShadowP.h"
#include "MenuGP.h"

typedef struct {
    XtPointer	extension;
} MenuClassPart;

typedef struct MenuClassRec {
    CoreClassPart	core_class;
    ShadowClassPart	shadow_class;
    MenuClassPart	menu_class;
} MenuClassRec;

extern MenuClassRec menuClassRec;

typedef struct MenuPart {
    Cursor		cursor;
    /* private */
    MenuGadget		*children;
    MenuGadget		current;
    Cardinal		num_children;
    Cardinal		num_slots;
    Dimension		pref_width;
    Dimension		pref_height;
    Boolean		active;
} MenuPart;

typedef struct MenuRec {
    CorePart		core;
    ShadowPart		shadow;
    MenuPart		menu;
} MenuRec;

#endif /* MenuP_h */
