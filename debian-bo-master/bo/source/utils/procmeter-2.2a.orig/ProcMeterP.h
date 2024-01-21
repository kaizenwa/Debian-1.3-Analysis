/***************************************
  $Header: /home/amb/procmeter/RCS/ProcMeterP.h 2.3 1996/09/21 20:43:03 amb Exp $

  ProcMeter Widget Private header file (for ProcMeter 2.2).
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1996 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#ifndef PROCMETERWIDGETP_H
#define PROCMETERWIDGETP_H    /*+ To stop multiple inclusions. +*/

#include "ProcMeter.h"

/*+ The Class Part of the Widget, shared among all instances of the ProcMeter Widget. +*/
typedef struct _ProcMeterClassPart
{
 int unused;                    /*+ Not used. +*/
}
ProcMeterClassPart;

/*+ The complete Class Record for the ProcMeter Widget, includes the Core Widget Class Part. +*/
typedef struct _ProcMeterClassRec
{
 CoreClassPart core_class;           /*+ The Core Widget Class Part. +*/
 ProcMeterClassPart procmeter_class; /*+ The ProcMeter Widget Class Part. +*/
}
ProcMeterClassRec;

/*+ The actual Class Record for the ProcMeter Widget. +*/
extern ProcMeterClassRec procMeterClassRec;

/*+ The ProcMeter Widget Part that is used in each of the ProcMeter Widgets. +*/
typedef struct _ProcMeterPart
{
 Pixel           foreground_pixel; /*+ The foreground colour (Set & Get via Xt). +*/
 GC              foreground_gc;    /*+ The graphics context for the foreground. +*/

 Pixel           grid_pixel;       /*+ The grid colour (Set & Get via Xt). +*/
 GC              grid_gc;          /*+ The graphics context for the grid. +*/
 Dimension       grid_height;      /*+ The height of the meter part. +*/
 Dimension       grid_start;       /*+ The start position of the meter part. +*/
 int             grid_min;         /*+ The minimum number of grid lines (Set & Get via Xt). +*/
 int             grid_max;         /*+ The maximum number of grid lines before removing them. +*/
 int             grid_drawn;       /*+ If 1 then draw as normal, if 0 never draw, if -1 draw only one line. +*/
 int             grid_num;         /*+ The actual number of grid lines. +*/

 Boolean         line_solid;       /*+ True if the area under the graph is to be filled (Set & Get via Xt). +*/

 char*           label;            /*+ The label for the Widget (Set & Get via Xt). +*/
 int             label_pos;        /*+ The position of the label (Set & Get via Xt). +*/
 XFontStruct*    label_font;       /*+ The font for the label (Set & Get via Xt). +*/
 Dimension       label_height;     /*+ The height of the label. +*/
 Dimension       label_x,label_y;  /*+ The position of the label. +*/

 unsigned short* data;             /*+ The data for the graph. +*/
 unsigned short  data_max;         /*+ The maximum data value. +*/
 unsigned int    data_num;         /*+ The number of data points. +*/
 int             data_index;       /*+ An index into the array for the new value. +*/
}
ProcMeterPart;

/*+ The complete Widget Record that is used per ProcMeter Widget. +*/
typedef struct _ProcMeterRec
{
 CorePart  core;                /*+ The Core Widget Part. +*/
 ProcMeterPart procmeter;       /*+ The ProcMeter Widget Part. +*/
}
ProcMeterRec;

#endif /* PROCMETERWIDGETP_H */
