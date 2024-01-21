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
#ifndef ManagerP_h
#define ManagerP_h

#include "Manager.h"
#include <X11/ConstrainP.h>

typedef struct ManagerConstraintsPart {
    Boolean		contain_horiz;
    Boolean		contain_vert;
} ManagerConstraintsPart;

typedef struct ManagerConstraintsRec {
    ManagerConstraintsPart	manager;
} ManagerConstraintsRec, *ManagerCons;    

typedef struct ManagerPart {
    XtCallbackList	resize_callback;
    Dimension		pref_width;
    Dimension		pref_height;
    /* private */
} ManagerPart;

typedef struct ManagerRec {
    CorePart		core;
    CompositePart	composite;
    ConstraintPart	constraint;
    ManagerPart		manager;
} ManagerRec;

typedef struct {
    XtPointer	extension;
} ManagerClassPart;

typedef struct ManagerClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ConstraintClassPart	constraint_class;
    ManagerClassPart	manager_class;
} ManagerClassRec;

extern ManagerClassRec managerClassRec;

#endif /* ManagerP_h */
