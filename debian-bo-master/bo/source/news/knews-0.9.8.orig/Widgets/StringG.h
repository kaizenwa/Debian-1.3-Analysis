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
#ifndef StringG_h
#define StringG_h

#ifndef XtCMargin
#define XtCMargin "Margin"
#endif
#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif
#ifndef XtCShadowWidth
#define XtCShadowWidth "ShadowWidth"
#endif
#ifndef XtCCommand
#define XtCCommand "Command"
#endif

#ifndef XtNleftMargin
#define XtNleftMargin "leftMargin"
#endif
#ifndef XtNrightMargin
#define XtNrightMargin "rightMargin"
#endif
#ifndef XtNshadowWidth
#define XtNshadowWidth "shadowWidth"
#endif
#ifndef XtNcommand
#define XtNcommand "command"
#endif

typedef struct StringGadgetClassRec*  StringGadgetClass;
typedef struct StringGadgetRec*       StringGadget;

extern WidgetClass stringGadgetClass;

extern char	*StringGadgetCommand(Widget);

#endif /* StringG_h */
