

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





#ifndef DSSCALEP_H
#define DSSCALEP_H

#include <X11/ConstrainP.h>
#include <X11/CompositeP.h>
#include <Xm/ManagerP.h>
#include "DSScale.h"

typedef struct _XmDoubleSliderScaleClassPart {
	int i;
} XmDoubleSliderScaleClassPart;

typedef struct _XmDoubleSliderScaleClassRec{
	CoreClassPart core_class;
	CompositeClassPart composite_class;
	ConstraintClassPart constraint_class;
	XmManagerClassPart  manager_class;
	XmDoubleSliderScaleClassPart dsscale_class;
}XmDoubleSliderScaleClassRec;

typedef struct _XmDoubleSliderScalePart {

XtCallbackList lower_drag_callback;
XtCallbackList upper_drag_callback;
XtCallbackList lower_value_changed_callback;
XtCallbackList upper_value_changed_callback;

Pixmap pixmap;
Pixmap slider;

GC foreground_GC;

Dimension slot_thickness;

Dimension slider_thickness;
Dimension slider_length;

int orientation;
int processing_direction;

Boolean show_values;

int minimum_value;
int maximum_value;

int lower_value;
int upper_value;

int lower_x;
int lower_y;
int upper_x;
int upper_y;

Position show_lower_value_x;
Position show_lower_value_y;
Position show_upper_value_x;
Position show_upper_value_y;

Boolean lower_slider_on;
Boolean upper_slider_on;

int offset_x;
int offset_y;

Dimension scale_width;
Dimension scale_height;

Dimension slider_size_x;
Dimension slider_size_y;

XmFontList font_list;

int scale_alignment;

} XmDoubleSliderScalePart;

typedef struct _XmDoubleSliderScaleRec {
   CorePart   core;
	CompositePart  composite;
	ConstraintPart constraint; 	
  	XmManagerPart   manager;
    XmDoubleSliderScalePart	dsscale; 
} XmDoubleSliderScaleRec;

#endif
