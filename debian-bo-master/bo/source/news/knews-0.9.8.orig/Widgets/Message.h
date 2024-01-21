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
#ifndef Message_h
#define Message_h

#ifndef XtCBuffer
#define XtCBuffer "Buffer"
#endif
#ifndef XtCCenter
#define XtCCenter "Center"
#endif
#ifndef XtCInternalHeight
#define XtCInternalHeight "InternalHeight"
#endif
#ifndef XtCInternalWidth
#define XtCInternalWidth "InternalWidth"
#endif
#ifndef XtCPreferredChars
#define XtCPreferredChars "PreferredChars"
#endif

#ifndef XtNbuffer
#define XtNbuffer "buffer"
#endif
#ifndef XtNcenter
#define XtNcenter "center"
#endif
#ifndef XtNpreferredChars
#define XtNpreferredChars "preferredChars"
#endif


typedef struct MessageClassRec*  MessageWidgetClass;
typedef struct MessageRec*       MessageWidget;

extern WidgetClass messageWidgetClass;

extern void MessageSetAndRedraw(Widget, char*, int);

#endif /* Message_h */
