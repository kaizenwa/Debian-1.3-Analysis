/********************************************************************

    File:       ups.h

    Purpose:    Read UPS status.
 
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

    $Log: ups.h,v $
    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/

/*  UPS States
 */
#define S_NOCHANGE -1  /*  don't know  */
#define S_OK        0
#define S_ONBAT     1
#define S_LOBAT     2
#define S_ERROR     3


/*  Status strings for logging.
 */
extern char *StatusString [];


/*-------------------------------------------------------------------*
     UPS_Open

     Open the UPS monitor device (serial port).

     Parameters:  DevName - Name of the device to open.

     Returns:     File descriptor that was opened.
 *-------------------------------------------------------------------*/
int UPS_Open (char *DevName);


/*-------------------------------------------------------------------*
     UPS_Setup

     Set up the port for monitoring.  Optionally kill the UPS
     power (only works if the UPS is on battery).

     Parameters:  fd   - File descriptor of monitor device.
                  kill - TRUE to shut down UPS power.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void UPS_Setup (int fd, int kill);


/*-------------------------------------------------------------------*
     UPS_Check

     Check the status of the UPS (Ok, On Battery, or Low Battery).
     
     Parameters:  fd   - File descriptor of monitor device.

     Returns:     S_* status code.
 *-------------------------------------------------------------------*/
int UPS_Check (int fd);
