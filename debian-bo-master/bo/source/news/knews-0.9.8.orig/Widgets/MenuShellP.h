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
#ifndef MenuShellP_h
#define MenuShellP_h

#include "MenuShell.h"
#include <X11/ShellP.h>

typedef struct {
    XtPointer	extension;
} MenuShellClassPart;

typedef struct MenuShellClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ShellClassPart		shell_class;
    OverrideShellClassPart	override_shell_class;
    MenuShellClassPart		menu_shell_class;
} MenuShellClassRec;

extern MenuShellClassRec	menuShellClassRec;

typedef struct {
    XtPointer	extension;
} MenuShellPart;

typedef struct MenuShellRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    OverrideShellPart	override;
    MenuShellPart	menu_shell;
} MenuShellRec;

#endif /* MenuShellP_h */
