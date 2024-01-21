/********************************************************************

    File:       common.c

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

    $Log: common.c,v $
    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/
#include <stdio.h>
#include <syslog.h>
#include "common.h"

static char *Version = "v1.0";

char *ProgName;   /*  Program name  */
int   Status;     /*  UPS Status (S_* codes)  */
int   IsDaemon;   /*  0 = not daemon, 1 = daemon, 2 = daemon with
                   *  log file open.  */


/*-------------------------------------------------------------------*
     LogError

     Log error messages to stderr or to syslog, depending on whether
     we are running as a daemon.

     Parameters:  Module - Module that is reporting the error.
                  Error  - Error message

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void LogError (char *Module, char *Error)
    {
    if (IsDaemon)
        {
        if (IsDaemon == 1)
            {
            openlog (ProgName, LOG_CONS, LOG_DAEMON);
            ++IsDaemon;
            }
        syslog (LOG_WARNING, "%s: %s", Module, Error);
        }
    else
        {
        printf ("%s: %s: %s\n", ProgName, Module, Error);
        }
    }


/*-------------------------------------------------------------------*
     Usage

     Print help & usage information.

     Parameters:  None

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void Usage (void)
    {
    printf ("Usage:\n");
    printf ("    %s [options] device | host\n", ProgName);
    printf ("Options:\n");
    printf (" -c count  Wait 'count' polls before shutdown (default 2).\n");
    printf (" -h        Print this help.\n");
    printf (" -i time   Set poll interval to 'time' seconds (default 10).\n");
    printf (" -k        Kill power now.\n");
    printf (" -l        Don't shut down until low battery.\n");
    printf (" -m        Disable master mode.\n");
    printf (" -p port   Use 'port' for remote status (default 401).\n");
    printf (" -s        Slave to host instead of device.\n");
    printf (" -t        Test mode, don't become daemon.\n");
    printf ("\n");
    printf ("Version ID = %s\n", Version);
    }
