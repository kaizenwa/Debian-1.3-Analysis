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
#ifndef Shadow_h
#define Shadow_h

#ifndef XtCAllocArmColor
#define XtCAllocArmColor "AllocArmColor"
#endif
#ifndef XtCAllocArmPixmap
#define XtCAllocArmPixmap "AllocArmPixmap"
#endif
#ifndef XtCAllocShadowColors
#define XtCAllocShadowColors "AllocShadowColors"
#endif
#ifndef XtCShadowWidth
#define XtCShadowWidth "ShadowWidth"
#endif
#ifndef XtCUseLineShadows
#define XtCUseLineShadows "UseLineShadows"
#endif

#ifndef XtNallocArmColor
#define XtNallocArmColor "allocArmColor"
#endif
#ifndef XtNallocArmPixmap
#define XtNallocArmPixmap "allocArmPixmap"
#endif
#ifndef XtNallocShadowColors
#define XtNallocShadowColors "allocShadowColors"
#endif
#ifndef XtNshadowWidth
#define XtNshadowWidth "shadowWidth"
#endif
#ifndef XtNuseLineShadows
#define XtNuseLineShadows "useLineShadows"
#endif

typedef struct ShadowClassRec*  ShadowWidgetClass;
typedef struct ShadowRec*       ShadowWidget;

extern WidgetClass shadowWidgetClass;

extern void ShadowRedrawWidget(Widget);

#endif /* Shadow_h */
