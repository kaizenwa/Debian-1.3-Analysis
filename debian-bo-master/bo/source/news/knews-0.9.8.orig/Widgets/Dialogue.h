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
#ifndef Dialogue_h
#define Dialogue_h

#ifndef XtCMessage
#define XtCMessage "Message"
#endif
#ifndef XtCBuffer
#define XtCBuffer "Buffer"
#endif
#ifndef XtCLabel
#define XtCLabel "Label"
#endif

#ifndef XtNmessage
#define XtNmessage "message"
#endif
#ifndef XtNbuffer
#define XtNbuffer "buffer"
#endif
#ifndef XtNleftLabel
#define XtNleftLabel "leftLabel"
#endif
#ifndef XtNmiddleLabel
#define XtNmiddleLabel "middleLabel"
#endif
#ifndef XtNrightLabel
#define XtNrightLabel "rightLabel"
#endif

typedef struct DialogueClassRec*  DialogueWidgetClass;
typedef struct DialogueRec*       DialogueWidget;

extern WidgetClass dialogueWidgetClass;

typedef enum {
    DialogueReplyLeft,
    DialogueReplyMiddle,
    DialogueReplyRight,
    DialogueReplyEnter,
    DialogueReplyTab,
    DialogueReplyClose
} DialogueReply;

typedef struct {
    DialogueReply	reply;
    String		buffer;
} DialogueReport;

extern Widget	DialogueGetTextField(Widget);

#endif /* Dialogue_h */
