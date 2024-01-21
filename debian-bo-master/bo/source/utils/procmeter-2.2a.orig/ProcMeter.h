/***************************************
  $Header: /home/amb/procmeter/RCS/ProcMeter.h 2.2 1996/09/21 20:42:59 amb Exp $

  ProcMeter Widget Public include file (for ProcMeter 2.2).
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1996 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#ifndef PROCMETERWIDGET_H
#define PROCMETERWIDGET_H    /*+ To stop multiple inclusions. +*/

void ProcMeterWidgetAddDatum(Widget pmw,unsigned short datum);

/*+ The ProcMeter Widget Class Record. +*/
extern WidgetClass procMeterWidgetClass;

/*+ An opaque reference to the ProcMeter Widget Class Record type. +*/
typedef struct _ProcMeterClassRec *ProcMeterWidgetClass;

/*+ An opaque reference to the ProcMeter Widget Record type. +*/
typedef struct _ProcMeterRec      *ProcMeterWidget;

/* The resource names */

#define XtNsolid         "solid"

#define XtNgrid          "grid"
#define XtNgridMin       "gridMin"

#define XtNlabelPosition "labelPosition"


#define XtCSolid         "Solid"

#define XtCGrid          "Grid"
#define XtCGridMin       "GridMin"

#define XtCLabelPosition "LabelPosition"

/* The options for label placement */

#define ProcMeterLabelTop     1
#define ProcMeterLabelNone    0
#define ProcMeterLabelBottom -1

/*+ The number of points per grid line. +*/
#define ProcMeterWidgetScale 1000

#endif /* PROCMETERWIDGET_H */
