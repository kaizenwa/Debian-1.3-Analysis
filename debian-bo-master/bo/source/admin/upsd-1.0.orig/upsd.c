/********************************************************************

    File:       upsd.c

    Purpose:    Monitor a UPS for power failure conditions.  The
                UPS can be a local device or we can monitor a master
                upsd over the network.  Can also be used to shut off
                UPS power.

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

    Author:     Bob Hauck (bobh@wasatch.com)

    $Log: upsd.c,v $
    Revision 1.2  1996/11/24 18:27:45  bobh
    Wasn't handling the case of poll time == 0 correctly.

    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/stat.h>
#include "common.h"
#include "net.h"
#include "ups.h"


/*  Flags set by command-line options
 */
static int   Mode_Kill;
static int   Mode_NoMaster;
static int   Mode_LowBat;
static int   Mode_Test;
static int   Mode_Port = 401;
static int   Mode_Delay = 10;
static int   Mode_Slave;
static int   Mode_Count = 2;


/*-------------------------------------------------------------------*
     NotifyInit

     Notify init of power fails by sending SIGPWR.  Local function,
     not exported.

     Parameters:  ok - TRUE if power is ok, FALSE if the UPS is
                       running on battery.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
static void NotifyInit (int ok)
    {
    int fd;

    /* Create the info file needed by init.
     */
    unlink (PWRSTAT);
    
    if ((fd = open (PWRSTAT, O_CREAT|O_WRONLY, 0644)) >= 0)
        {
        if (ok)
            write (fd, "OK\n", 3);
        else
            write (fd, "FAIL\n", 5);
        
        close (fd);
        }

    /*  Send the signal unless we're in test mode.
     */
    if (!Mode_Test)
        kill (1, SIGPWR);
    }


/*-------------------------------------------------------------------*
     EvalStatus

     Evaluate the UPS status and log changes.  If we are on battery,
     notify init to start a shutdown.  Will delay notification for
     Mode_Count update cycles so that users are not bothered by
     spurious notifications from short power glitches.  Local function,
     not exported.

     Parameters:  Fail - S_* UPS status code.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
static void EvalStatus (int Fail)
    {
    static int Count;
    static int LastFail = S_NOCHANGE;

    ++Count;
    
    if (LastFail != Fail)
        {
        LastFail = Fail;
        Count = 0;
        }

    if (Count == Mode_Count)
        {
        if (Fail != S_NOCHANGE)
            {
            LogError ("log", StatusString [Fail]);
        
            Status = Fail;
        
            switch (Fail)
                {
                case S_ONBAT:  /*  On battery  */
                    if (!Mode_LowBat)
                        NotifyInit (FALSE);
                    break;
                    
                case S_LOBAT:  /*  Low battery  */
                    NotifyInit (FALSE);
                    break;

                case S_OK:     /*  Power back  */
                    NotifyInit (TRUE);
                    break;
                }
            }
        }
    }


/*-------------------------------------------------------------------*
     Daemon

     Go into the background, become a daemon process.

     Parameters:  None.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
static void Daemon (void)
    {
    switch (fork ())
        {
        case 0: /* I am the child. */
            setsid ();
            chdir ("/");
            umask (0);
            ++IsDaemon;
            break;
            
        case -1: /* Failed to become daemon. */
            LogError ("daemon", "can't fork");
            exit (1);
            
        default: /* I am the parent. */
            exit (0);
        }
    }


/*-------------------------------------------------------------------*

  M A I N   P R O G R A M
  
 *-------------------------------------------------------------------*/
int main (int argc, char *argv [])
    {
    int           option;
    int           fd     = -1;
    unsigned long inaddr = (unsigned) -1;
        
    ProgName = argv [0];

    /*  Evaluate the command line.
     */
    while ((option = getopt (argc, argv, "ktli:shp:mc:")) > 0)
        {
        switch (option)
            {
            case 'k':    /*  Kill Power  */
                Mode_Kill = TRUE;
                break;

            case 't':    /*  Test Mode  */
                Mode_Test = TRUE;
                break;

            case 'l':    /*  Don't Shutdown until low bat  */
                Mode_LowBat = TRUE;
                break;

            case 'i':    /*  Poll Interval, < 0 means "one shot"  */
                if (optarg)
                    {
                    Mode_Delay = atoi (optarg);
                    if (Mode_Delay < 0)
                        {
                        Mode_Test = TRUE;
                        Mode_NoMaster = TRUE;
                        }
                    }
                else
                    LogError ("main", "bad poll delay spec");
                break;

            case 's':    /*  Slave mode  */
                Mode_Slave = TRUE;
                break;

            case '?':    /*  Help  */
            case 'h':
                Usage ();
                exit (0);
                break;

            case 'p':    /*  IP Port for Master  */
                if (optarg)
                    Mode_Port = atoi (optarg);
                else
                    LogError ("main", "bad port number");
                break;

            case 'm':    /*  Disable Master Mode  */
                Mode_NoMaster = TRUE;
                break;

            case 'c':
                if (optarg)
                    {
                    if ((Mode_Count = atoi (optarg)) < 0)
                        {
                        Mode_Count = 0;
                        LogError ("main", "count must be >= 0");
                        }
                    }
                else
                    LogError ("main", "bad poll count");
                break;
            }
        }

    unlink (PWRSTAT);

    if (optind >= argc)
        {
        LogError ("main", "no device specified");
        Usage ();
        exit (1);
        }

    /*  Set up the devices...
     */
    if (Mode_Slave)
        {
        if ((inaddr = NET_GetServerAddr (argv [optind])) == (unsigned) -1)
            exit (1);
        }
    else
        {
        if ((fd = UPS_Open (argv [optind])) < 0)
            exit (1);

        if (Mode_Kill)
            UPS_Setup (fd, 1);
        else
            UPS_Setup (fd, 0);
        }

    /*  Go into the background...
     */
    if (!Mode_Test)
        Daemon ();

    /*  Now start monitoring...
     */
    while (1)
        {
        if (Mode_Delay < 0)
            {
            /*  One-shot mode, useful in scripts...returns the
             *  ups status as an exit code > 100.
             */
            if (Mode_Slave)
                exit (NET_Check (inaddr, Mode_Port) + 100);
            else
                exit (UPS_Check (fd) + 100);
            }
        else
            {
            /*  Normal continuous mode...
             */
            if (Mode_Slave)
                EvalStatus (NET_Check (inaddr, Mode_Port));
            else
                EvalStatus (UPS_Check (fd));
             
            if (Mode_NoMaster || Mode_Slave)
                sleep (Mode_Delay);
            else
                NET_Serve (Mode_Port, Mode_Delay);
            }
        }

    /*  Error! (shouldn't happen)
     */
    return 1;
    }
