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
#ifndef StringGP_h
#define StringGP_h

#include "StringG.h"
#include "MenuGP.h"

typedef struct {
    XtPointer		extension;
} StringGadgetClassPart;

typedef struct StringGadgetClassRec {
    RectObjClassPart		rect_class;
    MenuGadgetClassPart		menu_g_class;
    StringGadgetClassPart	string_g_class;
} StringGadgetClassRec;

extern StringGadgetClassRec stringGadgetClassRec;

typedef struct {
    String	command;	/* user data */
    XFontStruct	*font;
    Pixel	foreground_pixel;
    Dimension	left_margin;
    Dimension	right_margin;
    Dimension	internal_height;
    Dimension	shadow_width;
    /* private data */
    GC		default_gc;
    GC		gray_gc;
    Pixmap	stipple;
} StringGadgetPart;

typedef struct StringGadgetRec {
    ObjectPart		object;
    RectObjPart		rectangle;
    MenuGadgetPart	menu_g;
    StringGadgetPart	string_g;
} StringGadgetRec;

#endif /* StringGP_h */
