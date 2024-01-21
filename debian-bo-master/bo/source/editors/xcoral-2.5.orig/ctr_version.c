/* ########################################################################

			     ctr_version.c

   File: ctr_version.c
   Path: /home/fournigault/c/X11/xcoral-2.31/ctr_version.c
   Description: 
   Created: Fri Jan 27 10:56:34 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:56:35 MET 1995
   Last maintained by: Lionel Fournigault

   RCS $Revision$ $State$
   

   ########################################################################

   Note: 

   ########################################################################

   Copyright (c) : Lionel Fournigault

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   ######################################################################## */


#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "main_text.h"

/*
**	Function name : CV_CheckIn
**
**	Description :
**	Input :
**	Output :
*/
void CV_CheckIn ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_check_in", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_CheckOutLocked
**
**	Description :
**	Input :
**	Output :
*/
void CV_CheckOutLocked ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_check_out_locked", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_CheckOutLocked
**
**	Description :
**	Input :
**	Output :
*/
void CV_CheckOutUnlocked ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_check_out_unlocked", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_CheckInAndOutUnlocked
**
**	Description :
**	Input :
**	Output :
*/
void CV_CheckInAndOutLocked ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_check_in_and_out_locked", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_CheckInAndOutUnlocked
**
**	Description :
**	Input :
**	Output :
*/
void CV_CheckInAndOutUnlocked ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_check_in_and_out_unlocked", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_ListDiff
**
**	Description :
**	Input :
**	Output :
*/
void CV_ListDiff ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_diff", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_ListLog
**
**	Description :
**	Input :
**	Output :
*/
void CV_ListLog ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_log", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_ListRepository
**
**	Description :
**	Input :
**	Output :
*/
void CV_ListRepository ( text )
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_repository", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_LockRevision
**
**	Description :
**	Input :
**	Output :
*/
void CV_LockRevision(text)
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_lock_revision", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_UnlockRevision
**
**	Description :
**	Input :
**	Output :
*/
CV_UnlockRevision(text)
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_unlock_revision", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}

/*
**	Function name : CV_Initialize
**
**	Description :
**	Input :
**	Output :
*/
CV_Initialize(text)
    Text *text;
{
  char *msg;
  msg = (char *) ie_call_function ( text, "cv_initialize", 0, 0);
  if ( msg != 0 )
    DisplayMessage ( text -> mwin, msg );
}


