/********************************************************************

    File:       net.h

    Purpose:    Support for remote monitoring of a UPS over the
                network.  Expects another copy of upsd to be running
                on the "master" server (the one connected to the
                UPS).

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

    $Log: net.h,v $
    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/


/*-------------------------------------------------------------------*
     NET_GetServerAddr

     Translate server name or IP in dotted-quad notation to an
     internet address in network format.

     Parameters:  Server - Server name or IP.

     Returns:     Server address or IN_ADDRNONE on failure.
 *-------------------------------------------------------------------*/
unsigned long NET_GetServerAddr (char *Server);


/*-------------------------------------------------------------------*
     NET_Check

     Get status of a remote UPS.

     Parameters:  Server - Server address in network format.
                  Port   - Remote IP port to connect to.

     Returns:     S_* status code.
 *-------------------------------------------------------------------*/
int NET_Check (unsigned long Server, int Port);


/*-------------------------------------------------------------------*
     NET_Serve

     Provide UPS status to remote hosts on the network.
     
     Parameters:  Port    - Port number to listen on.
                  MaxWait - Max time to wait for connection in secs.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
int NET_Serve (int Port, int MaxWait);
