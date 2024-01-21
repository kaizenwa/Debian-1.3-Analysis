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
#ifndef SeparatorG_h
#define SeparatorG_h

#ifndef XtCSize
#define XtCSize "Size"
#endif
#ifndef XtCShadowWidth
#define XtCShadowWidth "ShadowWidth"
#endif
#ifndef XtCInternalWidth
#define XtCInternalWidth "InternalWidth"
#endif
#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif

#ifndef XtNsize
#define XtNsize "size"
#endif
#ifndef XtNShadowWidth
#define XtNShadowWidth "ShadowWidth"
#endif

typedef struct SeparatorGadgetClassRec*  SeparatorGadgetClass;
typedef struct SeparatorGadgetRec*       SeparatorGadget;

extern WidgetClass separatorGadgetClass;

#endif /* SeparatorG_h */
