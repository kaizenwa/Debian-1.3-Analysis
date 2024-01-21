/********************************************************************

    File:       common.h

    Purpose:    Utility functions for upsd.

                Copyright 1996, Bob Hauck
                
                This program is free software; you can redistribute it
                and/or modify it under the terms of the GNU General Public
                License as published by the Free Software Foundation;
                either version 2 of the License, or (at your option) any
                later version.

                This program is distributed in the hope that it will be
                useful, but WITHOUT ANY WARRANTY; without even the implied
                warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
                PURPOSE.  See the GNU General Public License for more details.

                You should have received a copy of the GNU General Public
                License along with this program; if not, write to the Free
                Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
                USA.

    Language:   GCC 2.7.0

    Author:     Bob Hauck

    $Log: common.h,v $
    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/
#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE !FALSE
#endif

/*  This is the file needed by SysVInit
 */
#define PWRSTAT "/etc/powerstatus"


extern char *ProgName;   /*  Program name  */
extern int   Status;     /*  UPS Status (S_* codes)  */
extern int   IsDaemon;   /*  0 = not daemon, 1 = daemon, 2 = daemon with
                          *  log file open. */


/*-------------------------------------------------------------------*
     LogError

     Log error messages to stderr or to syslog, depending on whether
     we are running as a daemon.

     Parameters:  Module - Module that is reporting the error.
                  Error  - Error message

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void LogError (char *ModName, char *Error);


/*-------------------------------------------------------------------*
     Usage

     Print help & usage information.

     Parameters:  None

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void Usage (void);
