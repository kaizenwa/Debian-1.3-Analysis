/********************************************************************

    File:       net.c

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

    $Log: net.c,v $
    Revision 1.3  1996/12/03 03:08:56  bobh
    Handle SIGINT, use waitpid() on SIGTERM to make sure we
    get an zombies, use SO_REUSEADDR on the server socket.

    Revision 1.2  1996/11/24 18:27:09  bobh
    Forgot to handle interrupted system calls...we will see
    those when children die.

    Revision 1.1  1996/11/23 16:45:12  bobh
    Initial revision

**********************************************************************/
#include <errno.h>
#include <netdb.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "common.h"
#include "ups.h"

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif


/*  File descriptor for "master" mode.
 */
static int ServerFD = -1;


/*-------------------------------------------------------------------*
     SigTerm

     Handle SIGTERM, SIGINT and SIGCHLD.

     Parameters:  sig - Signal number being received.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void SigTerm (int Sig)
    {
    if (Sig == SIGCHLD)
        {
        signal (SIGCHLD, SigTerm);
        while (waitpid (-1, NULL, WNOHANG) > 0)
            ;
        }
    else
        {
        if (ServerFD >= 0)
            {
            close (ServerFD);
            exit (0);
            }
        }
    }


/*-------------------------------------------------------------------*
     TCPOpen

     Open a client-mode socket to communicate with the master
     server.  Local function, not exported.

     Parameters:  HostAddr - Remote host address in network format.

                  Port     - Remote port number to connect.

     Returns:     The open file descriptor or -1 on error.
 *-------------------------------------------------------------------*/
static int TCPOpen (unsigned long HostAddr, int Port)
    {
    int                fd;
    struct sockaddr_in server_addr;
    
    bzero (&server_addr, sizeof (struct servent));
    server_addr.sin_family = AF_INET;

    if (Port <= 0)
        {
        LogError ("tcp_open", "must specify a port");
        return -1;
        }

    server_addr.sin_port = htons (Port);

    if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0)
        {
        LogError ("tcp_open", "can't create socket");
        return -1;
        }

    bcopy (&HostAddr, &server_addr.sin_addr, sizeof (HostAddr));
        
    if (connect (fd,
                 (struct sockaddr *) &server_addr,
                 sizeof (server_addr))
        < 0)
        {
        LogError ("tcp_open", "can't connect to server");
        return -1;
        }

    return fd;
    }


/*-------------------------------------------------------------------*
     ReadLine

     Read a line of data from a remote host.  Local function, not
     exported.

     Parameters:  FD     - File descriptor of socket to read.
                  Buffer - Buffer in which to place data.
                  Len    - Length of Buffer.
                             or DNS hostname.

     Returns:     Count of bytes read.
 *-------------------------------------------------------------------*/
static int ReadLine (int FD, char *Buffer, int Len)
    {
    int  i;
    int  rc;
    char c;

    for (i = 0; i < Len; ++i)
        {
        if ((rc = read (FD, &c, 1)) == 1)
             {
             *Buffer++ = c;
             if (c == '\n')
                 break;
             }
        else if (rc == 0)
            {
            if (i == 1)
                return 0;
            else
                break;
            }
        else
            {
            return -1;
            }
        }
    
    *Buffer = 0;
    return i;
    }


/*-------------------------------------------------------------------*
     NET_GetServerAddr

     Translate server name or IP in dotted-quad notation to an
     internet address in network format.

     Parameters:  Server - Server name or IP.

     Returns:     Server address or IN_ADDRNONE on failure.
 *-------------------------------------------------------------------*/
unsigned long NET_GetServerAddr (char *Server)
    {
    struct hostent *hp;
    unsigned long  inaddr;
    
    if ((inaddr = inet_addr (Server)) == INADDR_NONE)
        {
        if ((hp = gethostbyname (Server)) == NULL)
            {
            LogError ("get_addr", "host name error");
            return -1;
            }

        bcopy (hp->h_addr, &inaddr, hp->h_length);
        }

    return inaddr;
    }


/*-------------------------------------------------------------------*
     NET_Check

     Get status of a remote UPS.

     Parameters:  Server - Server address in network format.
                  Port   - Remote IP port to connect to.

     Returns:     S_* status code.
 *-------------------------------------------------------------------*/
int NET_Check (unsigned long Server, int Port)
    {
    char       buffer [64];
    int        i;
    int status = S_NOCHANGE;
    int fd     = TCPOpen (Server, Port);

    if (fd >= 0)
        {
        if (ReadLine (fd, buffer, sizeof (buffer)) > 0)
            {
            for (i = S_OK; i <= S_ERROR; ++i)
                {
                if (strcmp (buffer, StatusString [i]) == 0)
                    {
                    status = i;
                    break;
                    }
                }

            if (i > S_ERROR)
                status = S_ERROR;
            }

        close (fd);
        }

    return status;
    }


/*-------------------------------------------------------------------*
     NET_Serve

     Provide UPS status to remote hosts on the network.
     
     Parameters:  Port    - Port number to listen on.
                  MaxWait - Max time to wait for connection in secs.

     Returns:     Nothing.
 *-------------------------------------------------------------------*/
void NET_Serve (int Port, int MaxWait)
    {
    fd_set         fdset;
    int            cli_len;
    int            newfd;
    int            sts;
    int            child;
    long           endtime;
    struct timeval timeout;
    static struct sockaddr_in serv_addr;
    static struct sockaddr_in cli_addr;

    if (ServerFD < 0)
        {
        /*  No socket is open yet
         */
        bzero (&serv_addr, sizeof (serv_addr));
        serv_addr.sin_family      = AF_INET;
        serv_addr.sin_addr.s_addr = htonl (INADDR_ANY);
        serv_addr.sin_port        = htons (Port);
        
        if ((ServerFD = socket (AF_INET, SOCK_STREAM, 0)) < 0)
            {
            LogError ("serve", "can't open server socket");
            exit (1);
            }

        sts = 1;
        
        setsockopt (ServerFD, SOL_SOCKET, SO_REUSEADDR, &sts, sizeof (sts));
        
        if (bind (ServerFD,
                  (struct sockaddr *) &serv_addr,
                  sizeof (serv_addr))
            < 0)
            {
            LogError ("serve", "can't bind server socket");
            close (ServerFD);
            ServerFD = -1;
            exit (1);
            }

        /*  Set up signal handlers and listen on the open socket.
         */
        listen (ServerFD, 5);
        signal (SIGCHLD, SigTerm);
        signal (SIGTERM, SigTerm);
        signal (SIGINT,  SigTerm);        
        }

    /*  Wait for a client to connect.  Don't wait longer than
     *  MaxWait seconds.
     */
    endtime = MaxWait + time (NULL);
    sts     = 1;

    while (sts > 0)
        {
        FD_ZERO (&fdset);
        FD_SET  (ServerFD, &fdset);

        timeout.tv_sec  = endtime - time (NULL);
        timeout.tv_usec = 0;

        if (timeout.tv_sec < 0)
            break;
        else
            sts = select (ServerFD + 1, &fdset, 0, 0, &timeout);

        if (sts < 0 && errno == EINTR)
            {
            /*  Interrupted system call...probably a child died.
             */
            sts = 1;
            continue;
            }
        else if (sts > 0)
            {
            /*  We have a client...fork and send the data.
             */
            cli_len = sizeof (cli_addr);
            newfd = accept (ServerFD, (struct sockaddr *) &cli_addr, &cli_len);
            child = fork ();
            
            if (child > 0)
                {
                close (newfd);
                }
            else if (child < 0)
                {
                LogError ("serve", "fork error");
                return;
                }
            else
                {
                write (newfd,
                       StatusString [Status],
                       strlen (StatusString [Status]));
                
                exit (0);
                }
            }
        }
    
    return;
    }
