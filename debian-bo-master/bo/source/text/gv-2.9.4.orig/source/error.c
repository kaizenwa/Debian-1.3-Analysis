/*
**
** error.c
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

#include <stdio.h>
#include <stdlib.h>

#include "paths.h"
#include INC_X11(Xos.h)
#include INC_X11(Intrinsic.h)

#include "gv.h"
#include "error.h"
#include "main_resources.h"
#include "main_globals.h"

char* 
open_fail_error(errornumber,error_str,file_name,show)
   int errornumber;
   char *error_str;
   char *file_name;
   int show;
{
   char buf[1024];

   BEGINMESSAGE(open_fail_error_message)
#  ifdef __DECC
      if (errornumber < __ERRNO_MAX) {
         if (show) sprintf(buf,"  %s: %s %s, %s\n",gv_application_name,error_str,file_name,strerror(errornumber));
         else      sprintf(buf,"%s %s: %s",error_str,file_name,strerror(errornumber));
      } else {
	 if (show) sprintf(buf,"  %s: %s %s\n",gv_application_name,error_str,file_name);
	 else      sprintf(buf,"%s %s",error_str,file_name);
      }
#  else
      if (errornumber <= sys_nerr) {
	 if (show) sprintf(buf,"  %s: %s %s, %s\n",gv_application_name,error_str,file_name,sys_errlist[errornumber]);
	 else      sprintf(buf,"%s %s: %s",error_str,file_name,sys_errlist[errornumber]);
      } else {
	 if (show) sprintf(buf,"  %s: %s %s\n",gv_application_name,error_str,file_name);
	 else      sprintf(buf,"%s %s",error_str,file_name);
      }
#  endif
   ENDMESSAGE(open_fail_error_message)
   if (show) { fprintf(stderr,buf); return NULL;             }
   else      {                      return XtNewString(buf); }
}

