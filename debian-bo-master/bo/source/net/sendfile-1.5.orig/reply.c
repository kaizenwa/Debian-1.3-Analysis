/*
 * File:	reply.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	10 Sep 95   Framstag	initial version
 *              21 Dec 95   Framstag	changed multi-line format
 *              17 Mar 95   Framstag	replaced uname() with gethostname()
 *               8 Apr 96   Framstag    changed signature command
 *              10 May 96   Framstag    uname() is used back again
 *              13 May 96   Framstag    uname() and gethostname()
 * 		24 Sep 96   Framstag	better host.domain-name determination
 *
 * Send the appropiate reply code and reply text conforming to NVT telnet
 * standard. Inspired by RFC-959 (ftp).
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#ifdef ULTRIX
  struct utsname
  { char    sysname[32];
    char    nodename[32];
    char    release[32];
    char    version[32];
    char    machine[32];
  };
  int uname (struct utsname *);
#else
  #include <sys/utsname.h>
#endif

#if !defined(HPUX)
  int gethostname(char*, int);
#endif

#include "config.h"		/* various #defines */
#include "system.h"		/* system information */
#include "net.h"		/* network routines */

/*
 * reply - send string conforming to NVT telnet standard
 *
 * INPUT:  rc  - reply code
 *
 * terminates program on fatal error
 */
void reply(int rc)
{ char *text,		/* reply code and reply text */
       host[FLEN+1];	/* hostname */
#ifndef NEXT
  struct utsname sysname;
#endif

  switch (rc)
  { case 200: text="200 Command ok.";
              break;
    case 201: text="201 File has been received correctly.";
              break;
    case 202: text="202 Command not implemented, superfluous at this site.";
              break;
    case 203: text="203 *schnuffel* *schnuffel* =:3";
              break;
    case 205: text="205 Non-ASCII character in command line ignored.";
              break;
    case 214: text="214-The following commands are recognized:\r\n"
                   "214-   FROM    <sender> [<real name>]\r\n"
                   "214-   TO      <recipient>\r\n"
	           "214-   FILE    <name>\r\n"
                   "214-   SIZE    <bytes to transfer> <original file size uncompressed>\r\n"
                   "214-   TYPE    BINARY|SOURCE|TEXT=<RFC-1345 character set name> [COMPRESSED|CRYPTED]\r\n"
                   "214-   DATE    <ISO-8601 date & time string (UTC)>\r\n"
                   "214-   SIGN    <pgp signature (armor)>\r\n"
                   "214-   ATTR    TAR|EXE|NONE\r\n"
                   "214-   MSG     <message>\r\n"
                   "214-   DEL\r\n"
                   "214-   RESEND\r\n"
                   "214-   DATA\r\n"
                   "214-   QUIT\r\n"
                   "214-All arguments have to be UTF-7 (RFC-1642) encoded.\r\n"
                   "214 You must specify at least FROM, TO, FILE, SIZE and "
                   "DATA to send a file.";
              break;
    case 215: text="215 sendfiled "VERSION" "REVISION" compiled on "SYSTEM;
              break;
    case 220: text=(char *) malloc(MAXLEN);
	      if (gethostname(host,FLEN)<0) 
                strcpy(host,"UNKNOWN");
              else
                if (!strchr(host,'.')) get_domainname(host);
#ifdef NEXT
	      sprintf(text,"220 %s "PROTOCOL" server (sendfiled "VERSION
	                   " on NeXTstep/Mach) ready.",host);
#elif defined(LINUX)
              if (uname(&sysname)<0)
                sprintf(text,"220 %s "PROTOCOL" server (sendfiled "VERSION
			     ") ready.",host);
              else
		if (strchr(sysname.nodename,'.'))
                  sprintf(text,"220 %s "PROTOCOL" server (sendfiled "VERSION
		               " on %s) ready.",
			  sysname.nodename,sysname.sysname);
              else
		sprintf(text,"220 %s.%s "PROTOCOL" server (sendfiled "VERSION
		             " on %s) ready.",
			sysname.nodename,sysname.domainname,sysname.sysname);
#else
              if (uname(&sysname)<0)
                sprintf(text,"220 %s "PROTOCOL" server (sendfiled "VERSION
			     ") ready.",host);
              else
		sprintf(text,"220 %s "PROTOCOL" server (sendfiled "VERSION
		             " on %s) ready.",host,sysname.sysname);
#endif
              break;

    case 221: text="221 Goodbye.";
              break;
    case 230: text="230 <xxx Bytes already received>";
              break;


    case 302: text="302 Header ok, send data.";
              break;


    case 401: text="401 Got unexpected EOF.";
              break;
    case 410: text="410 Spool directory does not exist.";
              break;
    case 411: text="411 Can't create user spool directory.";
              break;
    case 412: text="412 Can't write to user spool directory.";
              break;
    case 413: text="413 User spool file quota exceeded.";
              break;
    case 415: text="415 TCP error: received too few data.";
              break;
    case 421: text="421 Service currently not available.";
              break;
    case 451: text="451 Requested action aborted: server error.";
              break;
    case 452: text="452 Insufficient storage space.";
              break;
    case 453: text="453 Insufficient system resources.";
              break;

    case 500: text="500 Syntax error, command unrecognized.";
              break;
    case 501: text="501 Syntax error in parameters or arguments.";
              break;
    case 502: text="502 Command not implemented.";
              break;
    case 503: text="503 Bad sequence of commands.";
              break;
    case 504: text="504 Command not implemented for that parameter.";
              break;
    case 505: text="505 Missing argument.";
              break;
    case 506: text="506 Command line too long.";
              break;
    case 510: text="510 User has set a forward to xxx@yyy";
              text="510-This SAFT-server can only receive messages.\r\n"
                   "510 Send files to xxx@yyy";
              break;
    case 511: text="511 This SAFT-server can only receive files.\r\n";
              break;
    case 520: text="520 User unknown.";
              break;
    case 521: text="521 User is not allowed to receive files or messages.";
              break;
    case 522: text="522 User cannot receive messages.";
              break;
    case 523: text="523 You are not allowed to send to this user.";
              break;
    case 530: text="530 Can't delete file.";
              break;
    case 531: text="531 This file has been already received.";
              break;
    default:  text="599 Unknown error.";
  }

  /* send string conforming to NVT standard */
  printf("%s\r\n",text);
  fflush(stdout);

  /* if a fatal and non recoverable error occured terminate the program */
  if (text[0]=='4') exit(1);
}


/*
 * from RFC-959 (ftp):
 *
 * 	 110 Restart marker reply.
 *           In this case, the text is exact and not left to the
 *           particular implementation, it must read:
 *                MARK yyyy=mmmm
 *           Where yyyy is User-process data stream marker, and mmmm
 *           server's equivalent marker (note the spaces between markers
 *	     and "=").
 *       120 Service ready in nnn minutes.
 *       125 Data connection already open, transfer starting.
 *       150 File status okay, about to open data connection.
 *
 *       200 Command okay.
 *       202 Command not implemented, superfluous at this site.
 *       211 System status, or system help reply.
 *       212 Directory status.
 *       213 File status.
 *       214 Help message.
 *           On how to use the server or the meaning of a particular
 *           non-standard command.  This reply is useful only to the
 *           human user.
 *       215 NAME system type.
 *           Where NAME is an official system name from the list in the
 *           Assigned Numbers document.
 *       220 Service ready for new user.
 *       221 Service closing control connection.
 *           Logged out if appropriate.
 *       225 Data connection open, no transfer in progress.
 *       226 Closing data connection.
 *           Requested file action successful (for example, file
 *           transfer or file abort).
 *       227 Entering Passive Mode (h1,h2,h3,h4,p1,p2).
 *       230 User logged in, proceed.
 *       250 Requested file action okay, completed.
 *       257 "PATHNAME" created.
 *
 *       331 User name okay, need password.
 *       332 Need account for login.
 *       350 Requested file action pending further information.
 *
 *       421 Service not available, closing control connection.
 *           This may be a reply to any command if the service knows it
 *           must shut down.
 *       425 Can't open data connection.
 *       426 Connection closed, transfer aborted.
 *       450 Requested file action not taken.
 *           File unavailable (e.g., file busy).
 *       451 Requested action aborted: local error in processing.
 *       452 Requested action not taken.
 *           Insufficient storage space in system.
 *       500 Syntax error, command unrecognized.
 *           This may include errors such as command line too long.
 *       501 Syntax error in parameters or arguments.
 *       502 Command not implemented.
 *       503 Bad sequence of commands.
 *       504 Command not implemented for that parameter.
 *       530 Not logged in.
 *       532 Need account for storing files.
 *       550 Requested action not taken.
 *           File unavailable (e.g., file not found, no access).
 *       551 Requested action aborted: page type unknown.
 *       552 Requested file action aborted.
 *           Exceeded storage allocation (for current directory or dataset).
 *       553 Requested action not taken.
 *           File name not allowed.
 */
