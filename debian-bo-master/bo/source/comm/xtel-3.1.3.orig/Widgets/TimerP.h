/*	
 *   Copyright (C) 1995  Pierre Ficheux (pierre@rd.lectra.fr)
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */
#ident "$Id: TimerP.h,v 1.2 1997/01/08 14:43:29 pierre Exp $"

/* 
 * TimerP.h - Private definitions for Timer widget
 */

#ifndef _TimerP_h
#define _TimerP_h


#include "Timer.h"

/***********************************************************************
 *
 * Timer Widget Private Data
 *
 ***********************************************************************/


typedef enum {
    TimerOff, TimerOn
} timerState; 


/************************************
 *
 *  Class structure
 *
 ***********************************/


   /* New fields for the Timer widget class record */
typedef struct _TimerClass 
  {
    int makes_compiler_happy;  /* not used */
  } TimerClassPart;

   /* Full class record declaration */
typedef struct _TimerClassRec {
    CoreClassPart	core_class;
    TimerClassPart    	timer_class;
} TimerClassRec;

extern TimerClassRec timerClassRec;

/***************************************
 *
 *  Instance (widget) structure 
 *
 **************************************/

    /* New fields for the Timer widget record */
typedef struct {
    /* resources */
    XFontStruct	*font;
    Pixel	clock_foreground;
    Pixel	timer_foreground;
    Dimension	margin;
    int		min;
    int		sec;
    Boolean	timer_mode;
    Boolean	count_down;

    /* private state */
    int		min_init, sec_init;
    GC          clock_GC;
    GC          timer_GC;
    timerState	state; 
    XtIntervalId id;
    char value[256];
} TimerPart;



   /* Full widget declaration */
typedef struct _TimerRec {
    CorePart	core;
    TimerPart	timer;
} TimerRec;

#endif /* _XawTimerP_h */


