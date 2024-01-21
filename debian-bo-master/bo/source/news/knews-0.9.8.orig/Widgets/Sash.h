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
#ifndef Sash_h
#define Sash_h

#ifndef XtNcursor
#define XtNcursor "cursor"
#endif
#ifndef XtNpreferredWidth
#define XtNpreferredWidth "preferredWidth"
#endif
#ifndef XtNpreferredHeight
#define XtNpreferredHeight "preferredHeight"
#endif

typedef struct SashClassRec*  SashWidgetClass;
typedef struct SashRec*       SashWidget;

extern WidgetClass sashWidgetClass;

typedef struct {
    XEvent	*event;
    String	*params;
    Cardinal	*no_params;
} SashCallDataRec, *SashCallData;

#endif /* Sash_h */
