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
#ifndef MessageP_h
#define MessageP_h

#include "Message.h"
#include <X11/CoreP.h>

typedef struct {
    XtPointer	extension;
} MessageClassPart;

typedef struct MessageClassRec {
    CoreClassPart	core_class;
    MessageClassPart	message_class;
} MessageClassRec;

extern MessageClassRec messageClassRec;

typedef struct {
    Pixel		foreground_pixel;
    XFontStruct		*font;
    String		buffer;
    Dimension		internal_height;
    Dimension		internal_width;
    Dimension		pref_chars;
    Boolean		center;
    /* private data */
    Cardinal		rows;
    Cardinal		n_alloc;
    GC			default_gc;
} MessagePart;

typedef struct MessageRec {
    CorePart	core;
    MessagePart	message;
} MessageRec;

#endif /* MessageP_h */
