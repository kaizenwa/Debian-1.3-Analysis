/*
 * File:	sendmsg.c
 *
 * Author:	Ulli Horlacher (framstag@rus.uni-stuttgart.de)
 *
 * History:	
 * 
 *   11 Aug 95   Framstag	initial version
 *   12 Aug 95   Framstag	elm alias support
 *    2 Nov 95   Framstag	added minimal chat mode
 *   14 Nov 95   Framstag	added message receiving modes
 *   21 Dec 95   Framstag	avoid unnecessary error message while 
 *                              configuring the own tty
 *   21 Dec 95   Framstag	better server connect testing
 *   20 Feb 96   Framstag	changed msg-tty file to support NFS
 *    1 Apr 96   Framstag	added multiline mode
 *   17 Apr 96   Framstag	new error handling for open_connection
 *    2 May 96   Framstag	fixed stupid shutdown() programming bug
 *    3 May 96   Framstag	fixed bug with gethostname()
 *   24 May 96   Framstag	sendmsg no longer tries to configure the tty 
 *                              when there is none (sending via pipe)
 *   12 Aug 96   Framstag	no questions asked when in batch mode (no tty)
 *
 * The sendmessage client of the sendfile package.
 * Sends a single line text message to the SAFT-server of the destination
 * system to be displayed on the recipients terminal.
 *
 * Copyright © 1995 Ulli Horlacher
 * This file is covered by the GNU General Public License
 */


#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <pwd.h>
#include <sys/socket.h>

#include "config.h"		/* various #defines */
#include "string.h"		/* extended string functions */
#include "net.h"		/* the network routines */
#include "io.h"			/* (socket) read/write */
#include "message.h"		/* information, warning and error messages */
#include "utf7.h"		/* UTF-7 coding */
#include "destination.h"	/* check recipient and host */

#ifndef AIX
  #ifndef CONVEXOS
    FILE *popen(const char *, const char *);
  #endif
  int pclose(FILE *);
  #if defined(IRIX) || defined(LINUX)
    #include <getopt.h>
  #else
    int getopt(int, char * const *, const char *);
  #endif
#else
  #include "bsd.h"
#endif

#if !defined(HPUX)
  int gethostname(char *, int);
#endif

#ifdef NEXT
  int shutdown(int, int);
#endif

#ifdef LINUX
  int fileno(FILE *);
#endif

/* print short help usage text */
int usage();


/* global variables */
int verbose;		/* flag for verbose mode */
char *prg;		/* name of the game */


int main(int argc, char *argv[])
{ extern int
    optind;		/* number of scanned args with getopt */
  int
    sockfd,		/* socket file descriptor */
    chat,		/* flag for chat mode */
    receive,		/* receiving flag */
    multiline,		/* flag for sending a multiline message */
    opt;		/* option to test for */
  char
    *cp,		/* simple character pointer */
    recipient[FLEN], 	/* recipient at serverhost */
    user[FLEN], 	/* local user name */
    host[FLEN], 	/* name of serverhost */
    localhost[FLEN], 	/* name of the local host */
    msgcf[FLEN], 	/* message control file */
    oldmsgcf[FLEN], 	/* old message control file */
    line[MAXLEN],	/* input or output string */
    login[MAXLEN],	/* login user name */
    tmp[3*MAXLEN];	/* temporary string */
  char
    utf_msg[LEN_UTF],	/* msg in UTF-7 format */
    iso_msg[LEN_ISO];	/* msg in ISO Latin-1 format */
  FILE *pipe;		/* pipe file descriptor */

  chat=0;
  verbose=0;
  receive=0;
  multiline=0;
  *iso_msg=0;
  pipe=NULL;

  prg=argv[0];
  if ((cp=strrchr(prg,'/'))) prg=cp+1;

  /* scan the command line on options */
  while ((opt=getopt(argc,argv,"cvVmMlh?")) > 0)
  { switch (opt)
    { case ':':
      case 'h':
      case '?': exit(usage());
      case 'c': chat=1; break;
      case 'v': verbose=1; break;
      case 'm': receive=1 ; break;
      case 'M': receive=2 ; break;
      case 'l': multiline=1; break;
      case 'V': message(prg,'I',"version "VERSION" revision "REVISION); exit(0);
    }
  }

  /* too few arguments? */
  if (argc-optind<1 && receive==0) exit(usage());

  /* get the local host name */
  if (gethostname(localhost,FLEN-1)<0) strcpy(localhost,"localhost");

  /* get own user name, recipient name and host */
  destination(argc,argv,user,recipient,host);
  strcpy(login,user);
  if ((cp=strchr(login,' '))) *cp=0;

  /* test the local sendfiled */
  if (verbose) printf("testing local SAFT server:\n");
  sockfd=open_connection("127.0.0.1",SAFT);
  sock_getline(sockfd,line);
  if (verbose && *line) printf("%s\n",line);

  /* no local server? */
  if (!strbeq(line,"220 ") || !strstr(line,"SAFT"))
    message(prg,'W',"there is no local SAFT server - "
	            "you cannot receive messages");
  else
  {
    /* test if you can receive messages */
    sprintf(line,"FROM %s",login);
    sock_putline(sockfd,line);
    sock_getline(sockfd,line);
    if (verbose) printf("%s\n",line);
    sprintf(line,"TO %s",login);
    sock_putline(sockfd,line);
    sock_getline(sockfd,line);
    if (verbose) printf("%s\n",line);
    if (strbeq(line,"521 "))
    { errno=0;
      message(prg,'F',"You are not allowed to use the sendmsg service");
    }
    if (!strbeq(line,"200 "))
      message(prg,'W',"local server error - you cannot receive messages");
    
    if (isatty(fileno(stdin)))
    { 
      /* allow receiving of messages on this tty */
      sprintf(msgcf,"%s/%s/config/tty@%s",SPOOL,login,localhost);
      sprintf(oldmsgcf,"%s/%s/config/msg",SPOOL,login);
      sprintf(tmp,"( mesg y ; tty | tee %s > %s ) 2>&1",msgcf,oldmsgcf);
      pipe=popen(tmp,"r");

      /* error when configuring the tty? */
      if (fgets(line,MAXLEN-1,pipe))
      { if ((cp=strrchr(line,'\n'))) *cp=0;
	sprintf(tmp,"cannot configure your tty: %s",line);
	message(prg,'W',tmp);
      }
      else
      {
	if (receive==1 && argc-optind<1)
	  message(prg,'I',"receiving messages is now possible only on this tty");

	if (receive==2)
	{ unlink(msgcf);
	  unlink(oldmsgcf);
	  if (argc-optind<1)
	    message(prg,'I',"receiving messages is now possible on all ttys");
	}

      }
      pclose(pipe);
  
    }
  }

  if (receive>0 && argc-optind<1) exit(0);
  shutdown(sockfd,2);

  /* initiate the connection to the remote server */
  sockfd=open_connection(host,SAFT);
  errno=0;
  if (sockfd==-1) message(prg,'F',"cannot create a network socket");
  if (sockfd==-2) message(prg,'F',"cannot open connection to remote server");
  if (sockfd==-3) message(prg,'F',"remote server is unknown");

  /* name the local host */
  if (streq(host,"127.0.0.1")) strcpy(host,localhost);

  /* no remote server? */
  sock_getline(sockfd,line);
  if (verbose && *line) printf("%s\n",line);
  if (!strbeq(line,"220 ") || !strstr(line,"SAFT"))
  { errno=0;
    sprintf(tmp,"No SAFT server on port %d at %s",SAFT,host);
    message(prg,'F',tmp);
  }

  /* tell where to send to */
  sprintf(tmp,"sending to %s@%s",recipient,host);
  message(prg,'I',tmp);

  /* send FROM and TO headers */
  sprintf(tmp,"FROM %s",user);
  if (sendheader(sockfd,tmp)<0) exit(1);
  sprintf(tmp,"TO %s",recipient);
  if (sendheader(sockfd,tmp)<0) exit(1);

  /* send several lines at once? */
  if (multiline)
  { if (isatty(fileno(stdin))) 
      printf("Enter multiline message (max 10 lines!):\n");
    while (fgets(line,LEN_ISO-1,stdin))
    { if ((cp=strchr(line,'\n'))) *cp=0;

      /* message text too long? */
      if (strlen(iso_msg)+strlen(line)>LEN_ISO*.8)
      { errno=0;
	message(prg,'F',"message line too long");
      }

      if (*iso_msg)
	sprintf(iso_msg,"%s\r\n%s",iso_msg,line);
      else
	strcat(iso_msg,line);
    }

    /* encode to UTF-7 */
    iso2utf(utf_msg,iso_msg);

    /* message text too long? */
    if (strlen(utf_msg)>MAXLEN-10)
    { errno=0;
      message(prg,'F',"message line too long");
    }

    /* send the message */
    sprintf(tmp,"MSG %s",utf_msg);
    sendheader(sockfd,tmp);
  }
  else   /* single line or chat mode */
  {
    do
    { /* read the message */
      if (isatty(fileno(stdin))) printf("message: ");
      *iso_msg=0;
      fgets(iso_msg,LEN_ISO-1,stdin);

      /* message empty? */
      if (*iso_msg==0)
      { printf("\n");
	break;
      }
      if (*iso_msg=='\n') continue;

      /* strip off new line */
      cp=strrchr(iso_msg,'\n');
      if (cp && (cp!=(char *)iso_msg)) *cp=0;

      /* encode to UTF-7 */
      iso2utf(utf_msg,iso_msg);

      /* send the message */
      sprintf(tmp,"MSG %s",utf_msg);
      sendheader(sockfd,tmp);

    } while (chat);

  }

  /* close the connection */
  sock_putline(sockfd,"QUIT");
  getreply(sockfd);
  close(sockfd);

  exit(0);
}


/* void function for message() */
void cleanup() { }


/*
 * usage - print short help usage text
 */
int usage()
{ fprintf(stderr,"usage: %s [-v] user[@host]\n",prg);
  fprintf(stderr,"   or: %s [-mM]\n",prg);
  fprintf(stderr,"options: -v  verbose mode\n");
  fprintf(stderr,"         -m  receive messages only on this tty (default)\n");
  fprintf(stderr,"         -M  receive messages on other ttys, too\n");
  fprintf(stderr,"example: %s orakel@main01.bb.bawue.de\n",prg);
  return(2);
}
