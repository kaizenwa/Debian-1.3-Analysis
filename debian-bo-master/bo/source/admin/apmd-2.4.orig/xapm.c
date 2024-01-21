/* xapm.c -- 
 * Created: Tue Jan  9 21:23:09 1996 by r.faith@ieee.org
 * Revised: Sun Apr 21 16:37:44 1996 by r.faith@ieee.org
 * Copyright 1996 Rickard E. Faith (r.faith@ieee.org)
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: xapm.c,v 1.5 1996/04/21 20:49:57 faith Exp $
 * 
 */

#include <stdio.h>
#include <getopt.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Scrollbar.h>
#include "apm.h"

typedef struct _instance_variables {
   Pixel   highColor;
   Pixel   lowColor;
   Pixel   criticalColor;
   Pixel   chargingColor;
   Pixel   foreground;
   int     highValue;
   int     lowValue;
   String  geometry;
   int     delay;
   Boolean displayPercent;
} instance_variable_rec;

static XtAppContext          app_context;
static Widget                scrollbar;
static Widget                topLevel;
static Widget                command;
static XtIntervalId          timerId;
static instance_variable_rec iv;
static int                   debug;

static void update( XtPointer client_data, XtIntervalId *id )
{
   apm_info   i;
   char       buf[128];
   static int pixel           = -1;
   static int lpixel          = -1;
   static int bpixel          = -1;
   static int lastPercentage  = -1;
   static int lastMinutes     = -1;
   static int lastDisplay     = -1;
   static int count           = 0;

   apm_read( &i );

   if (iv.displayPercent) {
      if (lastDisplay != iv.displayPercent
	  || i.battery_percentage != lastPercentage) {
				/* lastPercentage updated at end */
	 sprintf( buf, "%d%%", i.battery_percentage );
	 XtVaSetValues( command, XtNlabel, buf, NULL );
      }
   } else {
      int minutes = i.using_minutes ? i.battery_time : i.battery_time / 60;

      if (lastDisplay != iv.displayPercent || lastMinutes != minutes) {
	 lastMinutes = minutes;
	 XtVaSetValues( command,
			XtNlabel, apm_time_nosec( minutes * 60 ), NULL );
      }
   }
   lastDisplay = iv.displayPercent;

   if (i.battery_percentage <= iv.lowValue) {
      if (pixel != iv.criticalColor)
	 XtVaSetValues( scrollbar, XtNforeground,
			pixel = iv.criticalColor, NULL );
      if (bpixel != iv.criticalColor)
	 XtVaSetValues( scrollbar, XtNborderColor,
			bpixel = iv.criticalColor, NULL );
   } else if (i.battery_percentage <= iv.highValue) {
      if (pixel != iv.lowColor)
	 XtVaSetValues( scrollbar, XtNforeground, pixel = iv.lowColor, NULL );
      if (bpixel != iv.foreground)
	 XtVaSetValues( scrollbar, XtNborderColor,
			bpixel = iv.foreground, NULL );
   } else {
      if (pixel != iv.highColor )
	 XtVaSetValues( scrollbar, XtNforeground, pixel = iv.highColor, NULL );
      if (i.battery_percentage == 100) {
	 if (bpixel != iv.highColor)
	    XtVaSetValues( scrollbar, XtNborderColor,
			   bpixel = iv.highColor, NULL );
      } else {
	 if (bpixel != iv.foreground)
	    XtVaSetValues( scrollbar, XtNborderColor,
			   bpixel = iv.foreground, NULL );
      }
   }
   
   if (debug) printf( "scrollbar color = %d\n", pixel );

   if (i.battery_status == 3) {
      if (lpixel != iv.chargingColor)
	 XtVaSetValues( command, XtNforeground,
			lpixel = iv.chargingColor, NULL );
   } else {
      if (i.battery_percentage < iv.lowValue && count++ % 2) {
	 if (lpixel != iv.criticalColor)
	    XtVaSetValues( command,
			   XtNforeground, lpixel = iv.criticalColor, NULL );
      } else {
	 if (lpixel != iv.foreground)
	    XtVaSetValues( command,
			   XtNforeground, lpixel = iv.foreground, NULL );
      }
   }

   if (debug) printf( "command color = %d, status = %d\n",
		      lpixel, i.battery_status );

   if (i.battery_percentage != lastPercentage)
      XawScrollbarSetThumb( scrollbar, 0.0,
			    i.battery_percentage < 0
			    ? 0.0
			    : i.battery_percentage / 100.0 );
   
   lastPercentage = i.battery_percentage;
   
   timerId = XtAppAddTimeOut( app_context, 1000 * iv.delay + 500,
			      update, app_context );
}

static void press( Widget w, XtPointer client_data, XtPointer call_data )
{
   iv.displayPercent = !iv.displayPercent;
   XtRemoveTimeOut( timerId );
   timerId = XtAppAddTimeOut( app_context, 0, update, app_context );
   if (debug) printf( "displayPercent = %d\n", iv.displayPercent );
}

static XrmOptionDescRec options[] = {
   { "-highcolor", "*highColor", XrmoptionSepArg, NULL },
   { "-lowcolor", "*lowColor", XrmoptionSepArg, NULL },
   { "-criticalcolor", "*criticalColor", XrmoptionSepArg, NULL },
   { "-chargingcolor", "*chargingColor", XrmoptionSepArg, NULL },
   { "-highvalue", "*highValue", XrmoptionSepArg, NULL },
   { "-lowvalue", "*lowValue", XrmoptionSepArg, NULL },
   { "-delay", "*delay", XrmoptionSepArg, NULL },
   { "-percent", "*percent", XrmoptionNoArg, (XtPointer)"true" },
};

#define offset(field) XtOffsetOf( instance_variable_rec, field )
static XtResource resources[] = {
   { "highColor", XtCForeground, XtRPixel, sizeof(Pixel),
     offset(highColor), XtRString, "green" },
   { "lowColor", XtCForeground, XtRPixel, sizeof(Pixel),
     offset(lowColor), XtRString, "yellow" },
   { "criticalColor", XtCForeground, XtRPixel, sizeof(Pixel),
     offset(criticalColor), XtRString, "red" },
   { "chargingColor", XtCForeground, XtRPixel, sizeof(Pixel),
     offset(chargingColor), XtRString, "blue" },
   { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground), XtRString, XtDefaultForeground },
   { "highValue", XtCValue, XtRInt, sizeof(int),
     offset(highValue), XtRImmediate, (XtPointer)50 },
   { "lowValue", XtCValue, XtRInt, sizeof(int),
     offset(lowValue), XtRImmediate, (XtPointer)10 },
   { "geometry", XtCString, XtRString, sizeof( String ),
     offset(geometry), XtRString, (XtPointer)"10x100" },
   { "delay", XtCValue, XtRInt, sizeof(int),
     offset(delay), XtRImmediate, (XtPointer)1 },
   { "percent", XtCValue, XtRBoolean, sizeof(Boolean),
     offset(displayPercent), XtRImmediate, (XtPointer)FALSE },
};

int main( int argc, char **argv )
{
   Cursor           cursor;
   int              c;
   Widget           form;
   XFontStruct      *fs;
   int              fontWidth, fontHeight;
   int              x = 0, y = 0, height = 10, width = 100;

   switch (apm_exists()) {
   case 1: fprintf( stderr, "No APM support in kernel\n" );  exit( 1 );
   case 2: fprintf( stderr, "Old APM support in kernel\n" ); exit( 2 );
   }

   topLevel = XtVaAppInitialize( &app_context, "XApm",
				 options, XtNumber( options ),
                                 &argc, argv, NULL, NULL );

   XtGetApplicationResources( topLevel,
			      &iv,
			      resources,
			      XtNumber( resources ),
			      NULL, 0 );

   if (iv.delay < 1) iv.delay = 1;

   XParseGeometry( iv.geometry, &x, &y, &width, &height );

   while ((c = getopt( argc, argv, "DV" )) != -1)
      switch (c) {
      case 'D': ++debug;          break;
      case 'V': 
	 fprintf( stderr, "apmd version %s\n", VERSION );
	 exit( 0 );
	 break;
      }

   if (debug) {
      printf( "highColor = %ld\n",     iv.highColor );
      printf( "lowColor = %ld\n",      iv.lowColor );
      printf( "criticalColor = %ld\n", iv.criticalColor );
      printf( "chargingColor = %ld\n", iv.chargingColor );
      printf( "foreground = %ld\n",    iv.foreground );
      printf( "highValue = %d\n",      iv.highValue );
      printf( "lowValue = %d\n",       iv.lowValue );
      printf( "geometry = %s\n",       iv.geometry );
   }

   cursor = XCreateFontCursor( XtDisplay( topLevel ), XC_top_left_arrow );

   form = XtVaCreateManagedWidget( "form",
				   formWidgetClass, topLevel,
				   XtNorientation, XtorientHorizontal,
				   XtNborderWidth, 0,
				   XtNdefaultDistance, 2,
				   NULL );
   
   command = XtVaCreateManagedWidget( "command",
				      commandWidgetClass, form,
				      XtNleft, XtChainLeft,
				      XtNhighlightThickness, 0,
				      XtNinternalHeight, 0,
				      XtNinternalWidth, 0,
				      XtNborderWidth, 0,
				      XtNlabel, "",
				      XtNresize, FALSE,
				      NULL );
   
   XtVaGetValues( command, XtNfont, &fs, NULL );
   fontWidth  = fs->max_bounds.width;
   fontHeight = fs->max_bounds.ascent + fs->max_bounds.descent;
   XtVaSetValues( command, XtNwidth, fontWidth * 4, NULL );

   if (debug) {
      int tmp = (width > fontWidth*4 - 6)
		? width - fontWidth * 4 - 6
		: fontWidth * 4;
      
      printf( "width = %d, using %d + %d = %d\n",
	      width, fontWidth * 4, tmp, fontWidth * 4 + tmp );
   }

   scrollbar = XtVaCreateManagedWidget( "scrollbar",
					scrollbarWidgetClass, form,
					XtNhorizDistance, 3,
					XtNfromHoriz, command,
					XtNorientation, XtorientHorizontal,
					XtNscrollHCursor, cursor,
					XtNthickness, fontHeight,
					XtNlength, (width > fontWidth*4 - 6)
					? width - fontWidth * 4 - 6
					: fontWidth * 4,
					NULL );

   XawScrollbarSetThumb( scrollbar, 0.0, 0.0 );
   XtVaSetValues( scrollbar,
		  XtNtranslations, XtParseTranslationTable( "" ), NULL );

   XtAddCallback( command, XtNcallback, press, NULL );

   XtRealizeWidget( topLevel );
   timerId = XtAppAddTimeOut( app_context, 0, update, app_context );
   XtAppMainLoop( app_context );

   return 0;
}
