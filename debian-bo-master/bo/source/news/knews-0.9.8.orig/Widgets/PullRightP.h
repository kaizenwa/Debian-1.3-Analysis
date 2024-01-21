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
#ifndef PullRightP_h
#define PullRightgP_h

#include "PullRight.h"
#include "StringGP.h"

typedef struct {
    XtPointer		extension;
} PullRightGadgetClassPart;

typedef struct PullRightGadgetClassRec {
    RectObjClassPart		rect_class;
    MenuGadgetClassPart		menu_g_class;
    StringGadgetClassPart	string_g_class;
    PullRightGadgetClassPart	pull_right_class;
} PullRightGadgetClassRec;

extern PullRightGadgetClassRec pullRightGadgetClassRec;

typedef struct {
    String	menu_name;
    Dimension	arrow_size;
    Dimension	arrow_offset;
    Dimension	arrow_shadow_width;
    /* private data */
    Widget	menu;
} PullRightGadgetPart;

typedef struct PullRightGadgetRec {
    ObjectPart		object;
    RectObjPart		rectangle;
    MenuGadgetPart	menu_g;
    StringGadgetPart	string_g;
    PullRightGadgetPart	pull_right;
} PullRightGadgetRec;

#endif /* PullRightP_h */
