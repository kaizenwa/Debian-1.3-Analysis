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
#ifndef ToggleG_h
#define ToggleG_h

#ifndef XtCToggleSize
#define XtCToggleSize "ToggleSize"
#endif
#ifndef XtCToggleOffset
#define XtCToggleOffset "ToggleOffset"
#endif
#ifndef XtCToggleShadowWidth
#define XtCToggleShadowWidth "ToggleShadowWidth"
#endif
#ifndef XtCSet
#define XtCSet "Set"
#endif

#ifndef XtNtoggleSize
#define XtNtoggleSize "toggleSize"
#endif
#ifndef XtNtoggleOffset
#define XtNtoggleOffset "toggleOffset"
#endif
#ifndef XtNtoggleShadowWidth
#define XtNtoggleShadowWidth "toggleShadowWidth"
#endif
#ifndef XtNset
#define XtNset "set"
#endif

typedef struct ToggleGadgetClassRec*  ToggleGadgetClass;
typedef struct ToggleGadgetRec*       ToggleGadget;

extern WidgetClass toggleGadgetClass;

extern void	ToggleGadgetSet(Widget, int);
extern int	ToggleGadgetGet(Widget);

#endif /* ToggleG_h */
