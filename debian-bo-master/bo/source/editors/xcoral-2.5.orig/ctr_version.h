/* ########################################################################

			     ctr_version.h

   File: ctr_version.h
   Path: /home/fournigault/c/X11/xcoral-2.31/ctr_version.h
   Description: 
   Created: Fri Jan 27 10:57:30 MET 1995
   Author: Lionel Fournigault
   Modified: Fri Jan 27 10:57:32 MET 1995
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


#ifndef _CTR_VERSION_h
#define _CTR_VERSION_h

FCT (void, QueryReplace, (Text *text) );
FCT (void, CV_CheckIn, (Text *text) );
FCT (void, CV_CheckOutLocked, (Text *text) );
FCT (void, CV_CheckOutUnlocked, (Text *text) );
FCT (void, CV_CheckInAndOutLocked, (Text *text) );
FCT (void, CV_CheckInAndOutUnlocked, (Text *text) );
FCT (void, CV_ListDiff, (Text *text) );
FCT (void, CV_ListLog, (Text *text) );
FCT (void, CV_ListRepository, (Text *text) );
FCT (void, CV_LockRevision, (Text *text) );
FCT (void, CV_UnlockRevision, (Text *text) );
FCT (void, CV_Initialize, (Text *text) );

#endif /* _CTR_VERSION_H_ */
