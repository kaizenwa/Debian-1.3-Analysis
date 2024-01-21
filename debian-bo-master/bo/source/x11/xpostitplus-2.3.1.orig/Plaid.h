/*
 * $Header: /home/src/X/xpostit/xpostit/Plaid.h,v 2.0 1995/03/27 18:57:18 mjhammel Exp $
 *
 * Plaid.h - public definitions for the plaid widget.
 *
 * Based on the Template widget from the X11R4 distribution.
 *
 * David A. Curry
 * SRI International
 * 333 Ravenswood Avenue
 * Menlo Park, CA 94025
 * davy@itstd.sri.com
 *
 * $Log: Plaid.h,v $
 * Revision 2.0  1995/03/27  18:57:18  mjhammel
 * Initial update to 2.0
 *
 * Revision 1.2  90/06/14  11:18:27  davy
 * Ported to X11 Release 4.
 * 
 * Revision 1.1  90/06/13  09:48:42  davy
 * Initial revision
 * 
 */
/* #include <X11/copyright.h> */

#ifndef _Plaid_h
#define _Plaid_h

/*
 * Resources:
 *
 * Name		     Class		RepType		Default Value
 * ----		     -----		-------		-------------
 * background	     Background		Pixel		XtDefaultBackground
 * border	     BorderColor	Pixel		XtDefaultForeground
 * borderWidth	     BorderWidth	Dimension	1
 * callback	     Callback		Callback	NULL
 * destroyCallback   Callback		Pointer		NULL
 * foreground	     Foreground		Pixel		XtDefaultForeground
 * height	     Height		Dimension	0
 * lowerCallback     Callback		Pointer		NULL
 * raiseCallback     Callback		Pointer		NULL
 * mappedWhenManaged MappedWhenManaged	Boolean		True
 * sensitive	     Sensitive		Boolean		True
 * width	     Width		Dimension	0
 * x		     Position		Position	0
 * y		     Position		Position	0
 */

/*
 * Declare specific PlaidWidget class and instance datatypes.
 */
typedef struct _PlaidClassRec*		PlaidWidgetClass;
typedef struct _PlaidRec*		PlaidWidget;

/*
 * Declare the class constant.
 */
extern WidgetClass			plaidWidgetClass;

/*
 * Declare a couple of new resources.
 */
#define XtNlowerCallback		"lowerCallback"
#define XtNraiseCallback		"raiseCallback"
#define XtNtearoffCallback		"tearoffCallback"
#define XtNhideCallback			"hideCallback"
#define XtNshowCallback			"showCallback"
#define XtNtshowCallback		"tshowCallback"
#define XtNtraiseCallback		"traiseCallback"
#define XtNquitCallback			"quitCallback"

#endif /* _Plaid_h */
