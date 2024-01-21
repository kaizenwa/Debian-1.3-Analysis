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
#ifndef MenuG_h
#define MenuG_h

#ifndef XtCLabel
#define XtCLabel "Label"
#endif

#ifndef XtNlabel
#define XtNlabel "label"
#endif
#ifndef XtNpostPopdownCallback
#define XtNpostPopdownCallback "postPopdownCallback"
#endif

typedef struct MenuGadgetClassRec*  MenuGadgetClass;
typedef struct MenuGadgetRec*       MenuGadget;

extern WidgetClass menuGadgetClass;

#endif /* MenuG_h */
