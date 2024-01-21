/* +-------------------------------------------------------------------+ */
/* | Copyright 1992, 1993, David Koblas (koblas@netcom.com)            | */
/* | Copyright 1995, 1996 Torsten Martinsen (bullestock@dk-online.dk)  | */
/* |                                                                   | */
/* | Permission to use, copy, modify, and to distribute this software  | */
/* | and its documentation for any purpose is hereby granted without   | */
/* | fee, provided that the above copyright notice appear in all       | */
/* | copies and that both that copyright notice and this permission    | */
/* | notice appear in supporting documentation.  There is no           | */
/* | representations about the suitability of this software for        | */
/* | any purpose.  this software is provided "as is" without express   | */
/* | or implied warranty.                                              | */
/* |                                                                   | */
/* +-------------------------------------------------------------------+ */

/* $Id: xpaint.h,v 1.7 1996/05/09 07:10:27 torsten Exp $ */

#if defined(HAVE_PARAM_H)
#include <sys/param.h>
#endif

#ifndef MIN
#define MIN(a,b)	(((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a,b)	(((a) > (b)) ? (a) : (b))
#endif
#ifndef ABS
#define ABS(a)		((a > 0) ? (a) : 0 - (a))
#endif
#ifndef SIGN
#define SIGN(a)		((a > 0) ? 1 : -1)
#endif

#ifdef __STDC__
#define CONCAT(a,b)	a##b
#else
#define CONCAT(a,b)	a/**/b
#endif

#ifdef AIXV3
#ifdef NULL
#undef NULL
#endif				/* NULL */
#define NULL 0
#endif				/* AIXV3 */

extern struct Global_s {
    struct {
	void *image;
	Colormap cmap;
	int width, height;
	Pixmap pix, mask;
    } region;
    Display *display;
    XtAppContext appContext;
    Boolean timeToDie;
    Time currentTime;
} Global;

typedef void *(*OperationFunc) (Widget,...);

typedef void *(*OperationAddProc) (Widget);
typedef void (*OperationRemoveProc) (Widget, void *);
typedef OperationFunc Operation_t;
typedef struct {
    OperationAddProc add;
    OperationRemoveProc remove;
} OperationPair;

#ifdef DEFINE_GLOBAL
#define EXTERN(var, val)	var = val ;
#else
#define EXTERN(var, val)	extern var ;
#endif

EXTERN(OperationPair * CurrentOp, NULL)
#ifdef DEFINE_GLOBAL
struct Global_s Global;
#endif

#define DEFAULT_TITLE	"Untitled"

#define AllButtonsMask \
	(Button1Mask|Button2Mask|Button3Mask|Button4Mask|Button5Mask)

/* gradient fill modes */
#define TFILL_RADIAL 0
#define TFILL_LINEAR 1
#define TFILL_CONE   2
#define TFILL_SQUARE 3

#define XYtoRECT(x1,y1,x2,y2,rect)					\
			(rect)->x = MIN(x1,x2); 			\
			(rect)->y = MIN(y1,y2);				\
			(rect)->width = MAX(x1,x2) - (rect)->x + 1;	\
			(rect)->height = MAX(y1,y2) - (rect)->y + 1;
