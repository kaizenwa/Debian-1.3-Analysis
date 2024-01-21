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
#ifndef ScrBar_h
#define ScrBar_h

#ifndef XtCAllowOff
#define XtCAllocOff "AllowOff"
#endif
#ifndef XtCCanvasLength
#define XtCCanvasLength "CanvasLength"
#endif
#ifndef XtCDecay
#define XtCDecay "Decay"
#endif
#ifndef XtCDelay
#define XtCDelay "Delay"
#endif
#ifndef XtCMinimumDelay
#define XtCMinimumDelay "MinimumDelay"
#endif
#ifndef XtCMinimumThumb
#define XtCMinimumThumb "MinimumThumb"
#endif
#ifndef XtCPushThumb
#define XtCPushThumb "PushThumb"
#endif
#ifndef XtCScrollCallback
#define XtCScrollCallback "ScrollCallback"
#endif
#ifndef XtCSliderLength
#define XtCSliderLength "SliderLength"
#endif
#ifndef XtCSliderPosition
#define XtCSliderPosition "SliderPosition"
#endif
#ifndef XtCStepSize
#define XtCStepSize "StepSize"
#endif
#ifndef XtCSyncScroll
#define XtCSyncScroll "SyncScroll"
#endif
#ifndef XtCVertical
#define XtCVertical "Vertical"
#endif

#ifndef XtNallowOff
#define XtNallowOff "allowOff"
#endif
#ifndef XtNcanvasLength
#define XtNcanvasLength "canvasLength"
#endif
#ifndef XtNdecay
#define XtNdecay "decay"
#endif
#ifndef XtNinitialDelay
#define XtNinitialDelay "initialDelay"
#endif
#ifndef XtNminimumDelay
#define XtNminimumDelay "minimumDelay"
#endif
#ifndef XtNminimumThumb
#define XtNminimumThumb "minimumThumb"
#endif
#ifndef XtNpushThumb
#define XtNpushThumb "pushThumb"
#endif
#ifndef XtNscrollCallback
#define XtNscrollCallback "scrollCallback"
#endif
#ifndef XtNsliderLength
#define XtNsliderLength "sliderLength"
#endif
#ifndef XtNsliderPosition
#define XtNsliderPosition "sliderPosition"
#endif
#ifndef XtNstepSize
#define XtNstepSize "stepSize"
#endif
#ifndef XtNsyncScroll
#define XtNsyncScroll "syncScroll"
#endif
#ifndef XtNvertical
#define XtNvertical "vertical"
#endif

typedef struct ScrBarClassRec*  ScrBarWidgetClass;
typedef struct ScrBarRec*       ScrBarWidget;

extern WidgetClass scrBarWidgetClass;

typedef struct {
    long	pos;
    long	shown;
    long	size;
} ScrollReport;

extern void ScrBarSetSliderPosition(Widget, long);
extern void ScrBarSetLengthsAndPos(Widget, long, long, long);

#endif /* ScrBar_h */
