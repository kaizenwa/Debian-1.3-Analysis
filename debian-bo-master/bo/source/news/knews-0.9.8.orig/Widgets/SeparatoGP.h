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
#ifndef SeparatoGP_h
#define SeparatoGP_h

#include "SeparatorG.h"
#include "MenuGP.h"

typedef struct {
    XtPointer		extension;
} SeparatorGadgetClassPart;

typedef struct SeparatorGadgetClassRec {
    RectObjClassPart		rect_class;
    MenuGadgetClassPart		menu_g_class;
    SeparatorGadgetClass	separator_g_class;
} SeparatorGadgetClassRec;

extern SeparatorGadgetClassRec separatorGadgetClassRec;

typedef struct {
    Dimension	size;
    Dimension	shadow_width;
    Dimension	internal_width;
    Dimension	internal_height;
    /* private data */
} SeparatorGadgetPart;

typedef struct SeparatorGadgetRec {
    ObjectPart		object;
    RectObjPart		rectangle;
    MenuGadgetPart	menu_g;
    SeparatorGadgetPart	separator_g;
} SeparatorGadgetRec;

#endif /* SeparatoGP_h */
