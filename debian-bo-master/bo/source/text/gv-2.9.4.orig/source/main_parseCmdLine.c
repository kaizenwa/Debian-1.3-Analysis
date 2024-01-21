/*
**
** main_parseCmdLine.c
**
** Copyright (C) 1995-1997 Johannes Plass
** 
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
** 
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
** 
** Author:   Johannes Plass (plass@thep.physik.uni-mainz.de)
**           Department of Physic
**           Johannes Gutenberg-University
**           Mainz, Germany
**
*/

/*
#define MESSAGES
*/
#include "message.h"

#include "config.h"

#include <stdlib.h>
#include <stdio.h>

#include "paths.h"
#include INC_X11(Intrinsic.h)

#include "d_memdebug.h"
#include "gv.h"
#include "main_resources.h"
#include "main_globals.h"

#ifdef VMS
#   include "strcasecmp.h"
#endif

#define IS_EQ(aaa,bbb) (!strcasecmp((aaa),(bbb)))
int
main_parseCmdLine (argc,argv)
   int  argc;
   char *argv[];
{
   int h_h=0;
   int h_help=0;
   int h_filename=0;
   int h_ad=0;
   int h_style=0;
   int h_gsSafer=0;
   int h_gsQuiet=0;
   int h_gsArguments=0;
   int i=0;
   int retval=0;

   BEGINMESSAGE(main_parseCmdLine)
   while ((++i)<argc) {
      SMESSAGE(argv[i])
      if      (!h_h       && IS_EQ("-h",argv[i]))         {
         INFMESSAGE(found -h)
         h_h = 1;
         retval=1;
      }
      else if (!h_h       && IS_EQ("-?",argv[i]))         {
         INFMESSAGE(found -h)
         h_h       = 1;
         retval=1;
      }
      else if (!h_help    && IS_EQ("-help",argv[i]))      {
         INFMESSAGE(found -help)
         h_help    = 1;
         retval=2;
      }
      else if (!h_gsSafer && IS_EQ("-Safer",argv[i]))   {
         INFMESSAGE(found -Safer)
         h_gsSafer = 1;
         gv_gs_safer = 1;
      }
      else if (!h_gsSafer && IS_EQ("-NoSafer",argv[i])) {
         INFMESSAGE(found -NoSafer)
         h_gsSafer = 1;
         gv_gs_safer = 0;
      }
      else if (!h_gsQuiet && IS_EQ("-Quiet",argv[i]))   {
         INFMESSAGE(found -Quiet)
         h_gsQuiet = 1;
         gv_gs_quiet = 1;
      }
      else if (!h_gsQuiet && IS_EQ("-NoQuiet",argv[i])) {
         INFMESSAGE(found -NoQuiet)
         h_gsQuiet = 1;
         gv_gs_quiet = 0;
      }
      else if (!h_gsArguments && IS_EQ("-Arguments",argv[i])) {
         INFMESSAGE(found -Arguments)
 	 if ((++i)<argc) {
	    h_gsArguments = 1;
 	    gv_gs_arguments = GV_XtNewString(argv[i]);
 	    SMESSAGE(gv_gs_arguments)
	 }
         SMESSAGE(gv_gs_arguments)
      }
      else if (!h_ad      && IS_EQ("-ad",argv[i]))        {
         INFMESSAGE(found -ad)
         if ((++i)<argc) {
#ifdef VMS
            char *pos;
            char tmp[256];
            sprintf(tmp,"%s;",argv[i]);
            pos = strchr(tmp,';');
            ++pos;
            *pos='\0';
            gv_ad_file = GV_XtNewString(tmp);
#else
            gv_ad_file = GV_XtNewString(argv[i]);
#endif
            SMESSAGE(gv_ad_file)
            h_ad = 1;
         }
      }
      else if (!h_style      && IS_EQ("-style",argv[i]))  {
         INFMESSAGE(found -style)
         if ((++i)<argc) {
            h_style = 1;
            gv_style_file = GV_XtNewString(argv[i]);
            SMESSAGE(gv_style_file)
         }
      }
      else if (!h_filename)                               {
         INFMESSAGE(found filename)
         h_filename=1;
         gv_filename = GV_XtNewString(argv[i]);
         SMESSAGE(gv_filename)
      }
   }
   ENDMESSAGE(main_parseCmdLine)
   return(retval);
}













