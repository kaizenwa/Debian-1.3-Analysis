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
#ifndef Notice_h
#define Notice_h

#ifndef XtCMessage
#define XtCMessage "Message"
#endif
#ifndef XtCLabel
#define XtCLabel "Label"
#endif
#ifndef XtCTimeout
#define XtCTimeout "Timeout"
#endif
#ifndef XtCWidget
#define XtCWidget "Widget"
#endif

#ifndef XtNmessage
#define XtNmessage "message"
#endif
#ifndef XtNleftLabel
#define XtNleftLabel "leftLabel"
#endif
#ifndef XtNleftKnapp
#define XtNleftKnapp "leftKnapp"
#endif
#ifndef XtNmiddleLabel
#define XtNmiddleLabel "middleLabel"
#endif
#ifndef XtNmiddleKnapp
#define XtNmiddleKnapp "middleKnapp"
#endif
#ifndef XtNrightLabel
#define XtNrightLabel "rightLabel"
#endif
#ifndef XtNrightKnapp
#define XtNrightKnapp "rightKnapp"
#endif
#ifndef XtNtimeout
#define XtNtimeout "timeout"
#endif

typedef struct NoticeClassRec*  NoticeWidgetClass;
typedef struct NoticeRec*       NoticeWidget;

extern WidgetClass noticeWidgetClass;

typedef enum {
    NoticeReplyLeft,
    NoticeReplyMiddle,
    NoticeReplyRight,
    NoticeReplyTimeout,
    NoticeReplyClose
} NoticeReply;

extern void	NoticeSetMessage(Widget, String);
extern void	NoticeSetLeftLabel(Widget, String);
extern Widget	NoticeMessageWidget(Widget);

#endif /* Notice_h */
