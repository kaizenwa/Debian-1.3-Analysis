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
#ifndef TextField_h
#define TextField_h

#ifndef XtCBorderIn
#define XtCBorderIn "BorderIn"
#endif
#ifndef XtCBuffer
#define XtCBuffer "Buffer"
#endif
#ifndef XtCDisplayCaret
#define XtCDisplayCaret "DisplayCaret"
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
#ifndef XtCPreferredLines
#define XtCPreferredLines "PreferredLines"
#endif
#ifndef XtCFocusRoot
#define XtCFocusRoot "FocusRoot"
#endif
#ifndef XtCHack
#define XtCHack "Hack"
#endif
#ifndef XtCDebug
#define XtCDebug "Debug"
#endif
#ifndef XtCEchoOff
#define XtCEchoOff "EchoOff"
#endif
#ifndef XtCSingleLine
#define XtCSingleLine "SingleLine"
#endif

#ifndef XtNborderIn
#define XtNborderIn "borderIn"
#endif
#ifndef XtBbuffer
#define XtNbuffer "buffer"
#endif
#ifndef XtNdisplayCaret
#define XtNdisplayCaret "displayCaret"
#endif
#ifndef XtNfocusColor
#define XtNfocusColor "focusColor"
#endif
#ifndef XtNpreferredChars
#define XtNpreferredChars "preferredChars"
#endif
#ifndef XtNpreferredLines
#define XtNpreferredLines "preferredLines"
#endif
#ifndef XtNhighlightForeground
#define XtNhighlightForeground "highlightForeground"
#endif
#ifndef XtNhighlightBackground
#define XtNhighlightBackground "highlightBackground"
#endif
#ifndef XtNfocusRoot
#define XtNfocusRoot "focusRoot"
#endif
#ifndef XtNtabCallback
#define XtNtabCallback "tabCallback"
#endif
#ifndef XtNfocusCallback
#define XtNfocusCallback "focusCallback"
#endif
#ifndef XtNfocusHack
#define XtNfocusHack "focusHack"
#endif
#ifndef XtNprintFocus
#define XtNprintFocus "printFocus"
#endif
#ifndef XtNechoOff
#define XtNechoOff "echoOff"
#endif
#ifndef XtNsingleLine
#define XtNsingleLine "singleLine"
#endif

typedef struct TextFieldClassRec*	TextFieldWidgetClass;
typedef struct TextFieldRec*		TextFieldWidget;

extern WidgetClass textFieldWidgetClass;

extern void	 TextFieldSetActive(Widget, int);
extern void	 TextFieldSetBuffer(Widget, char*);
extern char	*TextFieldGetBuffer(Widget);

#endif /* TextField_h */
