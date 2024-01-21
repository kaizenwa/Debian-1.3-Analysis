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
#ifndef FileSelP_h
#define FileSelP_h

#include "FileSel.h"
#include <X11/ShellP.h>

typedef struct {
    XtPointer	empty;
} FileSelClassPart;

typedef struct FileSelClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ShellClassPart		shell_class;
    WMShellClassPart		wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TopLevelShellClassPart	top_level_shell_class;
    FileSelClassPart		filesel_class;
} FileSelClassRec;

extern FileSelClassRec fileSelClassRec;

#define MAX_FILE_TYPE	5

typedef struct {
    XtCallbackList      callback;
    Cursor		cursor;
    Cursor		busy_cursor;
    String		directory;
    Dimension		pref_cols;
    Boolean		show_dot_files;
    /* private data */
    Widget		layout;
    Widget		dir_label;
    Widget		dir_field;
    Widget		file_label;
    Widget		file_field;
    Widget		list;
    Widget		knapp_cancel;
    Widget		knapp_choose;
    Widget		show_toggle;
    unsigned char	*types;
    Pixmap		pixmap[MAX_FILE_TYPE + 1];
} FileSelPart;

typedef struct FileSelRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TopLevelShellPart	top_level;
    FileSelPart		filesel;
} FileSelRec;

#endif /* FileSelP_h */
