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
#ifndef ToggleGP_h
#define ToggleGP_h

#include "ToggleG.h"
#include "StringGP.h"

typedef struct {
    XtPointer		extension;
} ToggleGadgetClassPart;

typedef struct ToggleGadgetClassRec {
    RectObjClassPart		rect_class;
    MenuGadgetClassPart		menu_g_class;
    StringGadgetClassPart	string_g_class;
    ToggleGadgetClassPart	toggle_g_part;
} ToggleGadgetClassRec;

extern ToggleGadgetClassRec toggleGadgetClassRec;

typedef struct {
    Dimension	toggle_size;
    Dimension	toggle_offset;
    Dimension	toggle_shadow_width;
    Boolean	set;
    /* private data */
} ToggleGadgetPart;

typedef struct ToggleGadgetRec {
    ObjectPart		object;
    RectObjPart		rectangle;
    MenuGadgetPart	menu_g;
    StringGadgetPart	string_g;
    ToggleGadgetPart	toggle_g;
} ToggleGadgetRec;

#endif /* ToggleGP_h */
