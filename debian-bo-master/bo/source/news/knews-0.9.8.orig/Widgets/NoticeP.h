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
#ifndef NoticeP_h
#define NoticeP_h

#include "Notice.h"
#include "CloseShP.h"

typedef struct {
    XtPointer	empty;
} NoticeClassPart;

typedef struct NoticeClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ShellClassPart		shell_class;
    WMShellClassPart		wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    CloseShellClassPart		close_shell;
    NoticeClassPart		notice_class;
} NoticeClassRec;

extern NoticeClassRec noticeClassRec;

typedef struct {
    XtCallbackList      callback;
    String		message;
    String		left_label;
    String		middle_label;
    String		right_label;
    long		timeout;
    /* read only */
    Widget		left_knapp;
    Widget		middle_knapp;
    Widget		right_knapp;
    /* private data */
    Widget		layout;
    Widget		message_widget;
    XtIntervalId	timer;
} NoticePart;

typedef struct NoticeRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;
    CloseShellPart	close_shell;
    NoticePart		notice;
} NoticeRec;

#endif /* NoticeP_h */

