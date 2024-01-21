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
#ifndef DialogueP_h
#define DialogueP_h

#include "Dialogue.h"
#include "CloseShP.h"

typedef struct {
    XtPointer	empty;
} DialogueClassPart;

typedef struct DialogueClassRec {
    CoreClassPart		core_class;
    CompositeClassPart		composite_class;
    ShellClassPart		shell_class;
    WMShellClassPart		wm_shell_class;
    VendorShellClassPart	vendor_shell_class;
    TransientShellClassPart	transient_shell_class;
    CloseShellClassPart		close_shell_class;
    DialogueClassPart		dialogue_class;
} DialogueClassRec;

extern DialogueClassRec dialogueClassRec;

typedef struct {
    XtCallbackList      callback;
    String		message;
    String		buffer;
    String		left_label;
    String		middle_label;
    String		right_label;
    /* private data */
    Widget		layout;
    Widget		message_widget;
    Widget		text_field;
    Widget		left_knapp;
    Widget		middle_knapp;
    Widget		right_knapp;
} DialoguePart;

typedef struct DialogueRec {
    CorePart		core;
    CompositePart	composite;
    ShellPart		shell;
    WMShellPart		wm;
    VendorShellPart	vendor;
    TransientShellPart	transient;
    CloseShellPart	close_shell;
    DialoguePart	dialogue;
} DialogueRec;

#endif /* DialogueP_h */

