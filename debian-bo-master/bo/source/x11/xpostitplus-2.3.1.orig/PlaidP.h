/*
 * $Header: /home/src/X/xpostit/xpostit/PlaidP.h,v 2.0 1995/03/27 18:57:18 mjhammel Exp $
 *
 * PlaidP.h - private definitions for the plaid widget.
 *
 * Based on the Template widget from the X11R4 distribution.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * $Log: PlaidP.h,v $
 * Revision 2.0  1995/03/27  18:57:18  mjhammel
 * Initial update to 2.0
 *
 * Revision 1.2  90/06/14  11:18:37  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:48:45  davy
 * Initial revision
 * 
 */
/* #include <X11/copyright.h> */

#ifndef _PlaidP_h
#define _PlaidP_h

#include <X11/CoreP.h>
#include "Plaid.h"

/*
 * The plaid part of the class record is not used.
 */
typedef struct {
	int		empty;
} PlaidClassPart;

/*
 * Declare the class record for the widget.
 */
typedef struct _PlaidClassRec {
	CoreClassPart	core_class;
	PlaidClassPart	plaid_class;
} PlaidClassRec;

/*
 * Declare the plaid class record type.
 */
extern PlaidClassRec	plaidClassRec;

/*
 * Resources specific to the plaid widget.
 */
typedef struct {
	Pixel		foreground;
	XtCallbackList	lower_callback;
	XtCallbackList	raise_callback;
	XtCallbackList	tearoff_callback;
	XtCallbackList  hide_callback;
	XtCallbackList  show_callback;
	XtCallbackList  tshow_callback;
	XtCallbackList  traise_callback;
	XtCallbackList  quit_callback;
} PlaidPart;

/*
 * Declare the widget type.
 */
typedef struct _PlaidRec {
	CorePart	core;
	PlaidPart	plaid;
} PlaidRec;

#endif /* _PlaidP_h */
