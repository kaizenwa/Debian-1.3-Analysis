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
#ifndef SashP_h
#define SashP_h

#include "Sash.h"
#include "ShadowP.h"

typedef struct {
    int empty;
} SashClassPart;

typedef struct SashClassRec {
    CoreClassPart       core_class;
    ShadowClassPart	shadow_class;
    SashClassPart	sash_class;
} SashClassRec;

extern SashClassRec sashClassRec;

typedef struct {
    XtCallbackList	callback;
    Cursor		cursor;
    /* private data */
    Dimension		pref_width;
    Dimension		pref_height;
} SashPart;

typedef struct SashRec {
    CorePart    core;
    ShadowPart	shadow;
    SashPart	sash;
} SashRec;

#endif /* SashP_h */
