/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 *
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990,1993 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  calldbx.c
 *
 *    Set up communication between dbx and xdbx using pseudo terminal, and
 *    call dbx.
 *
 *    open_master():	Open the master side of pty.
 *    open_slave(): 	Open the slave side of pty.
 *    calldbx(): 	Invoke dbx.
 *    create_io_window(): create an io window for gdb to use 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "global.h"
#if !(defined(OLDSUNOS) || defined(BSD))
#include <termio.h>
#else
#include <sgtty.h>
#endif

#ifdef CREATE_IO_WINDOW
#include	<sys/socket.h>
#include        <sys/un.h>
#endif /* CREATE_IO_WINDOW */

#ifdef SVR4
#define MASTER_CLONE "/dev/ptmx"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/stropts.h>
#include <signal.h>
#endif /* SVR4 */

#if (defined(BSD) && (BSD < 44)) || defined(ultrix)
#define OLDBSD
#endif
#if defined(TIOCSCTTY)
#define NEWBSD
#endif

extern char *progname;		/* (MJH) */

FILE   	    	*dbxfp = NULL;		/* file pointer to dbx */
int    	    	dbxpid = 0;		/* dbx process id */

static XtInputId	dbxInputId;		/* dbx input id */
#ifndef SVR4				/* (MJH) */
static char 	pty[11] = "/dev/pty??";	/* master side of pseudo-terminal */
static char 	tty[11] = "/dev/tty??";	/* slave side of pseudo-terminal */
#endif /* SVR4 */
extern char	*dbxprompt;

#ifdef CREATE_IO_WINDOW
char            iowintty[] = "/dev/ttyp0";
int             iowinpid = 0;
#endif /* CREATE_IO_WINDOW */
/*
 *  Xdbx talks to dbx through a pseudo terminal which is a pair of master
 *  and slave devices: /dev/pty?? and /dev/tty??, where ?? goes from p0 to
 *  sf (system dependent).  The pty is opened for both read and write.
 */
static int open_master()
{
    int master;
    
#ifdef SVR4				/* (MJH) Use STREAMS */

    if((master = open(MASTER_CLONE, O_RDWR)) < 0)
	perror(MASTER_CLONE);
    else
	return master;
#else
    int  i;
    char c;

#ifndef sco
	for (c='p'; c<'t'; c++) {
	for (i=0; i<16; i++) {
#else
	c = 'p';
	for (i=0; i<8; i++) {
#endif
	    pty[8] = c;
	    pty[9] = "0123456789abcdef"[i];
	    if ((master = open(pty, O_RDWR)) >= 0) 
		return (master); 
	}
#ifndef sco
	}
#endif
#endif /* SVR4 */

#ifdef GDB
    fprintf(stderr, "xxgdb: all ptys in use\n");
#else
    fprintf(stderr, "xdbx: all ptys in use\n");
#endif
    exit(1);
}

/*ARGSUSED*/
static int open_slave(master)
    int master;
{
    int slave;

#ifdef SVR4				/* (MJH) */
    char *slave_name = "unknown";
    extern char *ptsname(int master);
    void (*handler)();

    if(((handler = signal(SIGCHLD, SIG_DFL)) != SIG_ERR) &&
       (grantpt(master) == 0) &&
       (signal(SIGCHLD, handler) == SIG_DFL) &&
       (unlockpt(master) == 0) &&
       ((slave_name = ptsname(master)) != NULL) &&
       ((slave = open(slave_name, O_RDWR)) >= 0) &&
       (ioctl(slave, I_PUSH, "ptem") >= 0) &&
       (ioctl(slave, I_PUSH, "ldterm") >= 0))
	return slave;
    perror("Pseudo-tty slave");
    fprintf(stderr, "open: cannot open slave pty %s", slave_name);
    exit(1);
#else
    tty[8] = pty[8];
    tty[9] = pty[9];
    if ((slave = open(tty, O_RDWR)) < 0)
	    {
		perror(tty);
		exit(1);
	    }
    return slave;
#endif /* SVR4 */
}

#ifdef CREATE_IO_WINDOW 
/* use a separate io window to talk to gdb, so program output is not confused with gdb output. */
/* creates an io window which is the program xxgdbiowin running behind an 
 * xterm.  This function sets two global variables:
 * iowintty, a character array which is the resulting ptty of the xterm
 * iowinpid, an int which is the pid of xxgdbiowin
 */
void
create_io_window ()
{
    int pid = fork();
    if (pid == -1)
    {
	printf("unable to fork\n");
    }
    else if (pid)
    {  	/* parent */
	char ttypid[40];
	int sock;
	struct sockaddr_un name;

	sock = socket(AF_UNIX, SOCK_DGRAM, 0);
	name.sun_family = AF_UNIX;
	strcpy(name.sun_path, "/tmp/iowindowtty");
	bind(sock, (struct sockaddr*)&name, sizeof(struct sockaddr_un));
	read(sock, ttypid, 40);
	sscanf(ttypid, "%[a-z/0-9],%d", iowintty, &iowinpid);
	close(sock);
	unlink("/tmp/iowindowtty");
    }
    else
    {
	/* child */
	/* printf("xterm xterm -l -e xxgdbiowin\n");*/
	if (execlp("xterm", "xterm", "-e", "xxgdbiowin", 0))
	{
	    printf("exec of 'xterm -e xxgdbiowin' fails\n");
	    unlink("/tmp/iowindowtty");
	}
	}
}
#endif /* CREATE_IO_WINDOW */

/* ARGSUSED */
void calldbx(argc, argv)
int argc;
char *argv[];
{
/*
 * (JBL)10MAY91 : use sgttyb if generic BSD
 */
#if !(defined(OLDSUNOS) || defined(BSD))
    struct termio Termio;
#else
    struct sgttyb Termio;
#endif
    int  	  master;		/* file descriptor of master pty */
    int  	  slave; 		/* file descriptor of slave pty */
#ifdef OLDBSD
    int		  fd; 			/* file descriptor of controlling tty */
#endif
    char 	  *debugger; 		/* name of executable debugger */
    char	  errmsg[LINESIZ];

#ifdef GDB	/* for GDB, we use XXGDB_DEBUGGER instead */
    debugger = (char *) getenv("XXGDB_DEBUGGER");	/* first looks up env var */
#else
    debugger = (char *) getenv("DEBUGGER");	/* first looks up env var */
#endif

/* CRL mod 4 3/15/91 GWC if no env var then try app res for db_name */
    if (debugger == NULL &&
	app_resources.db_name &&
	strcmp(app_resources.db_name, "") != 0)
	debugger =  XtNewString(app_resources.db_name);
      
    if (debugger == NULL)
	debugger  = XtNewString(DEBUGGER);

/* CRL mod 4 3/15/91 GWC -  allow the user to specify a db_prompt */
    if (app_resources.db_prompt &&
	strcmp(app_resources.db_prompt, "") != 0)
	dbxprompt = XtNewString(app_resources.db_prompt);
  
    /* construct dbx prompt string based on the name of debugger invoked */
    if (dbxprompt == NULL) {
	dbxprompt = XtMalloc((4+strlen(debugger)) * sizeof(char));
	sprintf(dbxprompt, "(%s) ", debugger);
    }
    
	if (debug)
		fprintf(stderr,"debugger=\"%s\"\nprompt=\"%s\"\n",debugger,dbxprompt);
  
    master = open_master();

    dbxpid = fork();
    if (dbxpid == -1) {
	sprintf(errmsg, "%s error: Cannot fork %s\n", progname, debugger);	/* (MJH) */
	perror(errmsg);
	exit(1);
    }
    else if (dbxpid) { 
	/* 
	 * Parent : close the slave side of pty
	 *	    close stdin and stdout
	 *	    set the dbx file descriptor to nonblocking mode
	 *	    open file pointer with read/write access to dbx
	 *	    set line buffered mode
	 *	    register dbx input with X
	 */
	close(0);
	close(1);

#ifdef _POSIX_SOURCE
	fcntl(master, F_SETFL, O_NONBLOCK);
#else
	fcntl(master, F_SETFL, O_NDELAY);
#endif
	
	if((dbxfp = fdopen(master, "r+")) == NULL)	/* (MJH) */
	{
	    perror("Associating stdio stream with pty master");
	    exit(1);
	}
    
	/*  turn off stdio buffering  */
	setbuf(dbxfp, NULL);

	dbxInputId = XtAppAddInput(app_context, master, (XtPointer) XtInputReadMask, 
				   read_dbx, NULL);
    }
    else { 
	/* 
	 * Child : close master side of pty
	 * 	   redirect stdin, stdout, stderr of dbx to pty
	 *	   unbuffer output data from dbx
	 *	   exec dbx with arguments
	 */

	/* lose controlling tty */
#if defined(NEWBSD) || defined(SVR4) || defined(_POSIX_SOURCE)
	setsid();
#endif
#ifdef OLDBSD
	if ((fd = open("/dev/tty", O_RDWR)) > 0) {
	    ioctl(fd, TIOCNOTTY, 0);
	    close(fd);
	}
#endif

	slave = open_slave(master);
	close(master);

	/*
	 * Modify local and output mode of slave pty
	 */
	 
	/*
	 * (JBL)10MAY91 : use sgttyb if OLDSUN or generic BSD
	 */ 
#if !(defined(OLDSUNOS) || defined(BSD))
	ioctl(slave, TCGETA, &Termio);
	Termio.c_lflag &= ~ECHO;	/* No echo */
	Termio.c_oflag &= ~ONLCR;	/* Do not map NL to CR-NL on output */
	ioctl(slave, TCSETA, &Termio);
#else
	ioctl(slave, TIOCGETP, &Termio);
	Termio.sg_flags &= ~ECHO;	/* No echo */
	Termio.sg_flags &= ~CRMOD;	/* Do not map NL to CR-NL on output */
	ioctl(slave, TIOCSETP, &Termio);
#endif

	dup2(slave, 0);
	dup2(slave, 1);
	dup2(slave, 2);
	if (slave > 2)
	    close(slave);
	    
	fcntl(1, F_SETFL, O_APPEND);
	setbuf(stdout, NULL);

	/* gain controlling tty */
#ifdef NEWBSD
	ioctl(0, TIOCSCTTY, 0);
#endif

	/* flush stdin ! (for RS6000)  FIXME */

	argv[0] = debugger;

	if (debug) {
		int i=0;
		fprintf (stderr, "Forking \"%s",argv[i++]);
		while (argv[i]) {
			fprintf (stderr, " %s", argv[i++]);
		}
		fprintf (stderr, "\"\n");
	}

	execvp(debugger, argv);
	sprintf(errmsg, "%s error: cannot exec %s", progname, debugger);
	perror(errmsg);
	exit(1);
    }
}
