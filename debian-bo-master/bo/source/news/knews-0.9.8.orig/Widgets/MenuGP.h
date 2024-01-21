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
#ifndef MenuGP_h
#define MenuGP_h

#include "MenuG.h"
#include <X11/RectObjP.h>
#include "MenuP.h"

#define MenuGadgetChangeHlProc(g)    \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.change_hl)
#define MenuGadgetNotifyProc(g)      \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.notify)
#define MenuGadgetPostNotifyProc(g)  \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.post_notify)
#define MenuGadgetPopdownProc(g)     \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.popdown)
#define MenuGadgetSetActiveProc(g)   \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.set_active)
#define MenuGadgetIgnoreLeave(g)     \
(((MenuGadgetClass)(g)->object.widget_class)->menu_g_class.ignore_leave)

typedef void	(*MenuGadgetProc)(MenuGadget);
typedef int	(*MenuGadgetNotifyProc)(MenuGadget);
typedef void	(*MenuGadgetSetProc)(MenuGadget, int);

#define XtInheritChangeHl	((MenuGadgetProc)_XtInherit)
#define XtInheritPopdown	((MenuGadgetProc)_XtInherit)
#define XtInheritNotify		((MenuGadgetNotifyProc)_XtInherit)
#define XtInheritPostNotify	((MenuGadgetNotifyProc)_XtInherit)
#define XtInheritSetActive	((MenuGadgetSetProc)_XtInherit)

typedef struct {
    MenuGadgetProc		change_hl;
    MenuGadgetProc		popdown;
    MenuGadgetNotifyProc	notify;
    MenuGadgetNotifyProc	post_notify;
    MenuGadgetSetProc		set_active;
    Boolean			ignore_leave;
    XtPointer			extension;
} MenuGadgetClassPart;

typedef struct MenuGadgetClassRec {
    RectObjClassPart	rect_class;
    MenuGadgetClassPart	menu_g_class;
} MenuGadgetClassRec;

extern MenuGadgetClassRec menuGadgetClassRec;

typedef struct {
    XtCallbackList	callback;
    XtCallbackList	post_popdown_callback;
    String		label;
    /* private data */
    unsigned char	hl;
    unsigned char	inside;
    unsigned char	active;
} MenuGadgetPart;

typedef struct MenuGadgetRec {
    ObjectPart		object;
    RectObjPart		rectangle;
    MenuGadgetPart	menu_g;
} MenuGadgetRec;

#endif /* MenuGP_h */
