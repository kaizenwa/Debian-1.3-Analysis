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
#ident "$Id: Timer.h,v 1.2 1997/01/08 14:43:20 pierre Exp $"

#ifndef _Timer_h
#define _Timer_h


/* Timer widget resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------

*/

#define XtNclockForeground	"clockForeground"
#define XtNtimerForeground	"timerForeground"
#define XtNmargin		"margin"
#define XtNmin			"min"
#define XtNsec			"sec"
#define XtNtimerMode		"timerMode"
#define XtNcountDown		"countDown"

#define XtCClockForeground	"ClockForeground"
#define XtCTimerForeground	"TimerForeground"
#define XtCMin			"Min"
#define XtCSec			"Sec"
#define XtCTimerMode		"TimerMode"
#define XtCCountDown		"CountDown"

extern WidgetClass     timerWidgetClass;

typedef struct _TimerClassRec   *TimerWidgetClass;
typedef struct _TimerRec        *TimerWidget;

/* Public functions */
#ifdef _STDC_
void timerStartTimer (Widget w);
void timerStopTimer (Widget w);
void timerResetTimer (Widget w);
#else
void timerStartTimer ();
void timerStopTimer ();
void timerResetTimer ();
#endif /* _STDC_ */


#endif /* _Timer_h */
/* DON'T ADD STUFF AFTER THIS */
