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
#ifndef CloseShP_h
#define CloseShP_h

#include "CloseSh.h"
#include <X11/ShellP.h>

#define XtInheritCloseWindow	((XtWidgetProc)_XtInherit)

typedef struct {
    XtWidgetProc	close_window;
    XtPointer		empty;
} CloseShellClassPart;

typedef struct CloseShellClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ShellClassPart		shell_class;
    WMShellClassPart		wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    CloseShellClassPart		close_shell_class;
} CloseShellClassRec;

extern CloseShellClassRec closeShellClassRec;

typedef struct {
    Cursor		cursor;
    XtCallbackList	close_callback;
} CloseShellPart;

typedef struct CloseShellRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;
    CloseShellPart	close_shell;
} CloseShellRec;

#endif /* CloseShP_h */

