

/************************************************************************* 
 * Version 1.3  on  20.2.1996
 * (c) 1996 Pralay Kanti Dakua
 *     pralay@teil.soft.net      
 *     
 * Credits
 * -------
 * The keyboard translations are added by Andreas Zeller (zeller@comsoft.de).
 * -------
 *
 * This program is free software; permission hereby granted, free of
 * charge, to any person obtaining a copy of this software and 
 * associated documentations to copy, modify and  distribute it under 
 * the terms of the GNU General Public License as published by 
 * the Free Software Foundation, subject to the following conditions: 

 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.

 * AS THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
 * FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN
 * OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
 * PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED
 * OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS
 * TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU,  SHOULD THE
 * PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,
 * REPAIR OR CORRECTION.
 *
 **************************************************************************/




#ifndef DSSCALE_H
#define DSSCALE_H

#include <Xm/Xm.h>

#define XmNlowerDragCallback "lowerDragCallback"
#define XmCLowerDragCallback "LowerDragCallback"

#define XmNupperDragCallback "upperDragCallback"
#define XmCUpperDragCallback "UpperDragCallback"

#define XmNlowerValueChangedCallback "lowerValueChangedCallback"
#define XmCLowerValueChangedCallback "LowerValueChangedCallback"

#define XmNupperValueChangedCallback "upperValueChangedCallback"
#define XmCUpperValueChangedCallback "UpperValueChangedCallback"

#define XmNlowerValue "lowerValue"
#define XmCLowerValue "LowerValue"

#define XmNupperValue "upperValue"
#define XmCUpperValue "UpperValue"

#define XmNshowValues "showValues"
#define XmCShowValues "ShowValues"

#define XmNscaleAlignment "scaleAlignment"
#define XmCScaleAlignment "ScaleAlignment"
#define XmRScaleAlignment "ScaleAlignment"

extern WidgetClass xmDoubleSliderScaleWidgetClass;

typedef struct _XmDoubleSliderScaleClassRec *XmDoubleSliderScaleWidgetClass;
typedef struct _XmDoubleSliderScaleRec *XmDoubleSliderScaleWidget;

enum {
XmCR_LOWER_VALUE_CHANGED, XmCR_UPPER_VALUE_CHANGED,
XmCR_LOWER_DRAG, XmCR_UPPER_DRAG
};

 
typedef struct {
int reason;
XEvent *event;
int value;
} XmDoubleSliderScaleCallbackStruct;


extern void XmDoubleSliderScaleSetUpperValue(Widget, int);
extern void XmDoubleSliderScaleSetLowerValue(Widget, int);

extern void XmDoubleSliderScaleGetUpperValue(Widget, int *);
extern void XmDoubleSliderScaleGetLowerValue(Widget, int *);

#endif
