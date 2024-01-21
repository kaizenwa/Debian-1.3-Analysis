/********************************************************************

    File:       ups.c

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

    $Log: ups.c,v $
    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/
#include <sys/ioctl.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include "common.h"
#include "ups.h"

/*  Monitor these bits on the serial port.
 */
#define B_KILL  TIOCM_RTS         /*  Kill UPS    */
#define B_ONBAT TIOCM_CD          /*  On battery  */
#define B_LOBAT TIOCM_CTS         /*  Low Battery */
#define B_ERR   TIOCM_DSR         /*  Bad Cable   */

/*  Status strings for logging.
 */
char *StatusString [] =
{
    "OK",
    "ON BATTERY",
    "LOW BATTERY",
    "UPS_ERROR"
};


/*-------------------------------------------------------------------*
     UPS_Open

     Open the UPS monitor device (serial port).

     Parameters:  DevName - Name of the device to open.

     Returns:     File descriptor that was opened.
 *-------------------------------------------------------------------*/
int UPS_Open (char *DevName)
    {
    int fd;
    
    if ((fd = open (DevName, O_RDWR | O_NDELAY)) < 0)
        {
        LogError ("opendev", sys_errlist [errno]);
        }

    return fd;
    }


/*-------------------------------------------------------------------*
     UPS_Setup

     Set up the port for monitoring.  Optionally kill the UPS
     power (only works if the UPS is on battery).

     Parameters:  fd   - File descriptor of monitor device.
                  kill - TRUE to shut down UPS power.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void UPS_Setup (int fd, int kill)
    {
    int killpwr = B_KILL;

    if (!kill)
        {
        ioctl(fd, TIOCMBIC, &killpwr);
        sleep (1);
        }
    else
        {
        LogError ("kill", "Attempting to kill the power!\n");
        ioctl(fd, TIOCMBIC, &killpwr);
        sleep (5);
        ioctl (fd, TIOCMBIS, &killpwr);
        sleep (5);
        
        /* Hmmm..... If you have a power outage, you won't make it!
         */
        exit (0);
        }
    }


/*-------------------------------------------------------------------*
     UPS_Check

     Check the status of the UPS (Ok, On Battery, or Low Battery).
     
     Parameters:  fd   - File descriptor of monitor device.

     Returns:     S_* status code.
 *-------------------------------------------------------------------*/
int UPS_Check (int fd)
    {
    int flags;
    int status;
    
    ioctl (fd, TIOCMGET, &flags);
    status = (~flags & (B_ONBAT | B_LOBAT));

    if (~flags & B_ERR)
        return S_ERROR;
    
    switch (status)
        {
            /*  On battery, but battery ok
             */
        case B_ONBAT:
            status = S_ONBAT;
            break;

            /*  On battery, and battery is low
             */
        case B_LOBAT | B_ONBAT:
            status = S_LOBAT;
            break;

            /*  Everything is a-ok
             */
        default:
            status = S_OK;
        }

    return status;
    }
