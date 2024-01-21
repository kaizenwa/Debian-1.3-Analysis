/***************************************
  $Header: /home/amb/procmeter/RCS/procmeter.c 2.6 1996/09/15 10:34:27 amb Exp $

  ProcMeter - A simple performance monitor using /proc - Version 2.2.
  ******************/ /******************
  Written by Andrew M. Bishop

  This file Copyright 1994,95,96 Andrew M. Bishop
  It may be distributed under the GNU Public License, version 2, or
  any higher version.  See section COPYING of the GNU Public license
  for conditions under which this file may be redistributed.
  ***************************************/


#include <stdlib.h>
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Paned.h>

#include "ProcMeter.h"
#include "procmeter.h"


static void Usage(int verbose);
static void ParseOptions(int *argc,char** argv);
static void ParseMeters(int *argc,char** argv);
static void ResizePane(Widget w,XtPointer va,XEvent* e,Boolean* vb);
static void AddRemoveMeter(Widget widget,XtPointer clientData,XtPointer callData);
static void UpdateStats(XtPointer p,XtIntervalId i);


static XtAppContext app_context; /*+ The application context. +*/
static Display* display;        /*+ The display that the meter is on. +*/
static Widget pane,             /*+ The widget for the pane containing the meters. +*/
              menu;             /*+ The widget for the menu. +*/
static Dimension length;        /*+ The length of the pane. +*/

static Widget meters[N_STATS],  /*+ The list of meter Widgets. +*/
              menus[N_STATS];   /*+ The list of menu Widgets. +*/
static Statistics types[N_STATS]; /*+ The list of the types of each of the meters. +*/

static int n_meters=0;          /*+ The number of meters in existence. +*/

static int interval=1;          /*+ The interval between updates (in seconds). +*/

static int vertical=1;          /*+ If the meters are aligned vertically. +*/

/*++++++++++++++++++++++++++++++++++++++
  The main program.

  int main Returns a value to the operating system.

  int argc The number of command line arguments.

  char** argv The actual command line arguments.
  ++++++++++++++++++++++++++++++++++++++*/

int main(int argc,char** argv)
{
 Widget toplevel,menushell;

 Statistics i;

 /* Initialise all of the data in the statistics file. */

 VerifyStatistics();

 /* Parse the input */

 ParseOptions(&argc,argv);

 /* Initialise the display */

 toplevel=XtVaAppInitialize(&app_context,"ProcMeter",
                            NULL, (Cardinal)0,&argc,argv,NULL,
                            XtNtitle,"ProcMeter V2.2",
                            NULL);

 display=XtDisplay(toplevel);

 /* Create the pane widget */

 pane=XtVaCreateManagedWidget("pane",panedWidgetClass,toplevel,
                              XtNwidth, vertical?100:200,
                              XtNheight,vertical?200:100,
                              XtNorientation,vertical?XtorientVertical:XtorientHorizontal,
                              NULL);

 XtVaGetValues(pane,vertical?XtNwidth:XtNheight,&length,NULL);

 XtAddEventHandler(pane,StructureNotifyMask,False,(XtEventHandler)ResizePane,NULL);

 /* Create the menu widgets */

 menu=XtVaCreateManagedWidget("menu",menuButtonWidgetClass,pane,
                              XtNlabel,vertical?"ProcMeter":"P\nr\no\nc\nM\ne\nt\ne\nr",
                              XtNmenuName,"menushell",
                              XtNallowResize,False,
                              XtNresizeToPreferred,True,
                              XtNskipAdjust,True,
                              XtNshowGrip,False,
                              vertical?XtNwidth:XtNheight,length,
                              NULL);

 menushell=XtCreatePopupShell("menushell",simpleMenuWidgetClass,menu,NULL,0);

 for(i=0;i<N_STATS;i++)
    if(stats[i].avail)
      {
       menus[i]=XtVaCreateManagedWidget(stats[i].name,smeBSBObjectClass,menushell,
                                        XtNlabel,stats[i].name,
                                        XtNleftMargin,10,XtNheight,10,
                                        NULL);
       XtAddCallback(menus[i],XtNcallback,AddRemoveMeter,(XtPointer)i);
      }

 /* Parse the input */

 ParseMeters(&argc,argv);

 /* Show the widgets */

 XtRealizeWidget(toplevel);

 /* Run everything. */

 XtAppMainLoop(app_context);

 /* Close down. */

 XCloseDisplay(display);

 return(0);
}


/*++++++++++++++++++++++++++++++++++++++
  Print the usage of the program.

  int verbose If true then print the long message.
  ++++++++++++++++++++++++++++++++++++++*/

static void Usage(int verbose)
{
 int i;
 int c=0;

 if(verbose)
   {
    fprintf(stderr,"\nProcMeter Version 2.2\n\n");
    fprintf(stderr,"A simple X based meter for Linux using /proc.\n");
    fprintf(stderr,"(c) Andrew Bishop 1994,1995,1996. [amb@gedanken.demon.co.uk]\n\n");
   }

 fprintf(stderr,"Usage: ProcMeter [-h] [-update u] [-horizontal|-vertical] [-force-spkt]\n");
 fprintf(stderr,"                 [option1] [option2] ...\n");
 for(i=0;i<N_STATS;i++)
    if(stats[i].avail)
      {
       if(!c)
          fprintf(stderr,"Statistic options - ");
       if(c && !(c%5))
          fprintf(stderr,"\n                    ");
       fprintf(stderr," %-11s",stats[i].name);
       c++;
      }
 fprintf(stderr,"\n\n");

 exit(1);
}


/*++++++++++++++++++++++++++++++++++++++
  Parses the input and prints a usage message if required.

  int *argc The arguments.

  char** argv The arguments.
  ++++++++++++++++++++++++++++++++++++++*/

static void ParseOptions(int *argc,char** argv)
{
 int j;

 for(j=0;j<*argc;j++)
    if(!strcmp(argv[j],"-h"))
       Usage(1);

 for(j=1;j<*argc;j++)
    if(!strcmp(argv[j],"-update"))
      {
       if(j==*argc-1)
         {fprintf(stderr,"ProcMeter: The -update option requires a time value\n");Usage(0);}
       interval=atoi(argv[j+1]);
       if(interval<=0 || interval>60)
         {fprintf(stderr,"ProcMeter: The -update option requires a sensible time value\n");Usage(0);}
       for(j+=2;j<*argc;j++)
          argv[j-2]=argv[j];
       *argc-=2;
      }

 for(j=1;j<*argc;j++)
    if(!strcmp(argv[j],"-horizontal"))
      {
       vertical=0;
       for(j+=1;j<*argc;j++)
          argv[j-1]=argv[j];
       *argc-=1;
      }

 for(j=1;j<*argc;j++)
    if(!strcmp(argv[j],"-vertical"))
      {
       vertical=1;
       for(j+=1;j<*argc;j++)
          argv[j-1]=argv[j];
       *argc-=1;
      }

 for(j=1;j<*argc;j++)
    if(!strcmp(argv[j],"-force-spkt"))
      {
       stats[SPKT].avail=stats[SPKT_RX].avail=stats[SPKT_TX].avail=1;
       for(j+=1;j<*argc;j++)
          argv[j-1]=argv[j];
       *argc-=1;
      }
}


/*++++++++++++++++++++++++++++++++++++++
  Start of all the meters specified on the command line.

  int *argc The number of command line arguments.

  char** argv The actual command line arguments.
  ++++++++++++++++++++++++++++++++++++++*/

static void ParseMeters(int *argc,char** argv)
{
 int error=0;
 int j;
 Statistics i;

 /* Start the update timer. */

 UpdateStats(NULL,0);

 /* Create the meter widgets */

 for(j=1;j<*argc;j++)
   {
    for(i=0;i<N_STATS;i++)
       if(!strcmp(argv[j],stats[i].name))
         {
          if(stats[i].avail)
             AddRemoveMeter(NULL,(XtPointer)i,NULL);
          else
            fprintf(stderr,"ProcMeter: Option %s is unavailable\n",argv[j]);
          break;
         }
    if(i==N_STATS)
       error=fprintf(stderr,"ProcMeter: Unrecognized option %s\n",argv[j]);
   }

 if(error)
    Usage(0);
}


/*++++++++++++++++++++++++++++++++++++++
  Called when a menu item is chosen.

  Widget widget The widget causing the callback.

  XtPointer clientData The type to change.

  XtPointer callData Not used.
  ++++++++++++++++++++++++++++++++++++++*/

static void AddRemoveMeter(Widget widget,XtPointer clientData,XtPointer callData)
{
 int i;
 Statistics selected=(Statistics)clientData;
 static Pixmap marker=(Pixmap)0;

 if(!marker)
   {
    GC tempgc;
    XGCValues gcxval;

    marker=XCreatePixmap(XtDisplay(pane),RootWindowOfScreen(XtScreen(pane)),10,10,1);

    gcxval.foreground=0;
    gcxval.background=0;
    tempgc=XCreateGC(XtDisplay(pane),marker,GCForeground|GCBackground,&gcxval);

    XFillRectangle(XtDisplay(pane),marker,tempgc,0,0,10,10);
    XSetForeground(XtDisplay(pane),tempgc,(unsigned long)~0);
    XFillArc(XtDisplay(pane),marker,tempgc,1,1,7,7,0,360*64);

    XFreeGC(XtDisplay(pane),tempgc);
   }

 for(i=0;i<n_meters;i++)
    if(types[i]==selected)       /* Remove a meter. */
      {
       Widget dead=meters[i];

       for(;i<n_meters;i++)
         {
          types[i]=types[i+1];
          meters[i]=meters[i+1];
         }

       XtDestroyWidget(dead);

       n_meters--;
      }

 if(i==n_meters)    /* Add a meter */
   {
    char label[20];

    types[n_meters]=selected;
    sprintf(label,"%s (%s)",stats[selected].name,stats[selected].units);

    meters[n_meters]=XtVaCreateManagedWidget(stats[selected].name,procMeterWidgetClass,pane,
                                             vertical?XtNwidth:XtNheight,length,
                                             XtNlabel,label,
                                             XtNallowResize,True,
                                             XtNshowGrip,False,
                                             NULL);

    n_meters++;

    XtVaSetValues(menus[selected],XtNleftBitmap,marker,NULL);
   }
 else
    XtVaSetValues(menus[selected],XtNleftBitmap,NULL,NULL);

 ResizePane(NULL,NULL,NULL,NULL);
}


/*++++++++++++++++++++++++++++++++++++++
  The function called for each update interval.

  XtPointer p Not used.

  XtIntervalId i Not used.
  ++++++++++++++++++++++++++++++++++++++*/

static void UpdateStats(XtPointer p,XtIntervalId i)
{
 static XtIntervalId id;

 id=XtAppAddTimeOut(app_context,1000*(unsigned)interval,(XtTimerCallbackProc)UpdateStats,NULL);

 if(n_meters)
   {
    int i;

    GetStatistics();

    for(i=0;i<n_meters;i++)
       ProcMeterWidgetAddDatum(meters[i],(unsigned short)(ProcMeterWidgetScale*CurrentValue(types[i])));
   }
}


/*++++++++++++++++++++++++++++++++++++++
  A callback that is activated by a resize event on the parent pane.

  Widget w The widget that caused the callback.

  XtPointer va Not used.

  XEvent* e The event that requires action.

  Boolean* vb Not used.

  This function is only ever called from the Xt Intrinsics routines.
  ++++++++++++++++++++++++++++++++++++++*/

static void ResizePane(Widget w,XtPointer va,XEvent* e,Boolean* vb)
{
 Dimension fsize,msize,size;
 int i;

 XtVaGetValues(pane,vertical?XtNwidth:XtNheight,&length,NULL);

 if(n_meters)
   {
    XtVaGetValues(pane,vertical?XtNwidth:XtNheight,&length,vertical?XtNheight:XtNwidth,&fsize,NULL);
    XtVaGetValues(menu,vertical?XtNheight:XtNwidth,&msize,NULL);

    size=(int)(fsize-msize)/n_meters;

    XtVaSetValues(menu,vertical?XtNwidth:XtNheight,length,vertical?XtNheight:XtNwidth,msize,NULL);
    for(i=0;i<n_meters;i++)
       XtVaSetValues(meters[i],vertical?XtNwidth:XtNheight,length,vertical?XtNheight:XtNwidth,size,NULL);
   }
}
