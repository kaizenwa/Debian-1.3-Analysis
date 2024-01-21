/***********************************************************************
 *
 * MemStripChart Widget
 * from StripChart Widget derived 
 *
 * Author:  Hans-Helmut B"uhmann 20. Jan. 1996
 *
 ***********************************************************************/

#ifndef _XawMemStripChartP_h
#define _XawMemStripChartP_h

#include "MemStripChart.h"
#include <X11/Xaw/SimpleP.h>

#define NO_GCS 0
#define FOREGROUND (1 << 0)
#define HIGHLIGHT  (1 << 1)
#define CODE       (1 << 2)
#define SHARED     (1 << 3)
#define BUFFER     (1 << 4)
#define FREE       (1 << 5)
#define SWAP       (1 << 6)
#define ALL_GCS    (FOREGROUND | HIGHLIGHT | CODE | SHARED | BUFFER | FREE | SWAP)

/* New fields for the memStripChart widget instance record */

typedef struct {
    Pixel	fgpixel;	/* color index for text */
    Pixel	hipixel;	/* color index for lines */
    Pixel       codepixel;      /* color index for code memory */
    Pixel       cachedpixel;    /* color index for cached memory */
    Pixel       bufferpixel;    /* color index for buffer memory */
    Pixel       freepixel;      /* color index for free memory */
    Pixel       swappixel;      /* color index for used swap memory */

    GC	fgGC;		/* graphics context for fgpixel */
    GC	hiGC;		/* graphics context for hipixel */
    GC  codeGC; 	/* graphics context for codepixel */        
    GC  cachedGC;	/* graphics context for cachedpixel */
    GC  bufferGC;	/* graphics context for bufferpixel */
    GC  freeGC;	        /* graphics context for freepixel */
    GC  swapGC;	        /* graphics context for swappixel */

    /* start of graph stuff */
    
    int	update;		/* update frequence */
    int	scale;		/* scale factor */
    int	min_scale;	/* smallest scale factor */
    int	interval;	/* data point interval */
    XPoint * points ;	/* Poly point for repairing graph lines. */
    double max_value;	/* Max Value in window */
    MemStripChartCallbackData 
        valuedata[1024]; /* record of data points */
    XtIntervalId interval_id;
    XtCallbackList get_value; /* proc to call to fetch load pt */
    int jump_val;		/* Amount to jump on each scroll. */
} MemStripChartPart;

/* Full instance record declaration */
typedef struct _MemStripChartRec {
   CorePart core;
   SimplePart simple;
   MemStripChartPart mem_strip_chart;
} MemStripChartRec;

/* New fields for the StripChart widget class record */
typedef struct {int dummy;} MemStripChartClassPart;

/* Full class record declaration. */
typedef struct _MemStripChartClassRec {
   CoreClassPart core_class;
   SimpleClassPart simple_class;
   MemStripChartClassPart mem_strip_chart_class;
} MemStripChartClassRec;

/* Class pointer. */
extern MemStripChartClassRec MemstripChartClassRec;

#endif /* _XawMemStripChartP_h */
