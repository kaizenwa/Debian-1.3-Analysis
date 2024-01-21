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
#ifndef MenuKnapp_h
#define MenuKnapp_h

#ifndef XtCArrowSize
#define XtCArrowSize "ArrowSize"
#endif
#ifndef XtCArrowOffset
#define XtCArrowOffset "ArrowOffset"
#endif
#ifndef XtCMenuName
#define XtCMenuName "MenuName"
#endif
#ifndef XtCMultiClickTime
#define XtCMultiClickTime "MultiClickTime"
#endif

#ifndef XtNarrowSize
#define XtNarrowSize "arrowSize"
#endif
#ifndef XtNarrowOffset
#define XtNarrowOffset "arrowOffset"
#endif
#ifndef XtNarrowShadowWidth
#define XtNarrowShadowWidth "arrowShadowWidth"
#endif
#ifndef XtNmenuName
#define XtNmenuName "menuName"
#endif
#ifndef XtNmultiClickTime
#define XtNmultiClickTime "multiClickTime"
#endif
#ifndef XtNpopdownTime
#define XtNpopdownTime "popdownTime"
#endif

typedef struct MenuKnappClassRec*	MenuKnappWidgetClass;
typedef struct MenuKnappRec*		MenuKnappWidget;

extern WidgetClass menuKnappWidgetClass;

#endif /* MenuKnapp_h */
