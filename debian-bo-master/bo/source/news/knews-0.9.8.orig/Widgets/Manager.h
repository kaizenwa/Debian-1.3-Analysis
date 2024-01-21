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
#ifndef Manager_h
#define Manager_h

#ifndef XtCContain
#define XtCContain "Contain"
#endif
#ifndef XtCPreferredHeight
#define XtCPreferredHeight "PreferredHeight"
#endif
#ifndef XtCPreferredWidth
#define XtCPreferredWidth "PreferredWidth"
#endif

#ifndef XtNcontainHoriz
#define XtNcontainHoriz "containHoriz"
#endif
#ifndef XtNcontainVert
#define XtNcontainVert "containVert"
#endif
#ifndef XtNpreferredHeight
#define XtNpreferredHeight "preferredHeight"
#endif
#ifndef XtNpreferredWidth
#define XtNpreferredWidth "preferredWidth"
#endif
#ifndef XtNresizeCallback
#define XtNresizeCallback "resizeCallback"
#endif

typedef struct ManagerRec		*ManagerWidget;
typedef struct ManagerClassRec		*ManagerWidgetClass;

extern WidgetClass managerWidgetClass;

extern void	Remanage(Widget);

#endif /* Manager_h */
