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
#ifndef Login_h
#define Login_h

#ifndef XtCMessage
#define XtCMessage "Message"
#endif

#ifndef XtNmessage
#define XtNmessage "message"
#endif
#ifndef XtNuserNameBuffer
#define XtNuserNameBuffer "userNameBuffer"
#endif
#ifndef XtNpassWordBuffer
#define XtNpassWordBuffer "passWordBuffer"
#endif
#ifndef XtNuserNameLabel
#define XtNuserNameLabel "userNameLabel"
#endif
#ifndef XtNpassWordLabel
#define XtNpassWordLabel "passWordLabel"
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
#ifndef XtNfieldWidth
#define XtNfieldWidth "fieldWidth"
#endif

typedef struct LoginClassRec*	LoginWidgetClass;
typedef struct LoginRec*	LoginWidget;

extern WidgetClass loginWidgetClass;

typedef enum {
    LoginReplyLeft,
    LoginReplyMiddle,
    LoginReplyRight,
    LoginReplyEnter,
    LoginReplyClose
} LoginReply;

typedef struct {
    LoginReply	reply;
    char	*username;
    char	*password;
} LoginReport;

#endif /* Login_h */
