/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char copyright[] = "Copyright (c) 1990 Regents of the University of California.\nAll rights reserved.\n";
static char SccsId[] = "@(#)@(#)pop_xmit.c	2.1  2.1 3/18/91";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#ifdef POPSCO
# define __SCO_WAIT3__
# include <fcntl.h>
#endif

#include <sys/wait.h>

#if defined(SOLARIS2) || defined(UNIXWARE) || defined(AIX) || defined(PTX) \
	|| defined(AUX)
#include <sys/stat.h>
#include <fcntl.h>
#endif

#include "popper.h"

/*
 *  xmit:   POP XTND function to receive a message from 
 *          a client and send it in the mail
 */

pop_xmit (p)
POP     *   p;
{
    FILE                *   tmp;                    /*  File descriptor for 
                                                        temporary file */
    int			    tfn;		    
    char                    buffer[MAXLINELEN];     /*  Read buffer */
    char                    temp_xmit[MAXDROPLEN];  /*  Name of the temporary 
                                                        filedrop */
#ifdef NEXT
    union	wait	    stat;
#else
    int			    stat;
#endif
    PID_T                   id, pid;

    /*  Create a temporary file into which to copy the user's message */

    strncpy(temp_xmit, POP_TMPXMIT, sizeof(temp_xmit));
#ifdef DEBUG
    if(p->debug)
        pop_log(p,POP_DEBUG,
            "Creating temporary file for sending a mail message \"%s\"",
                temp_xmit);
#endif
    if (((tfn=mkstemp(temp_xmit)) == -1) ||
	((tmp=fdopen(tfn, "w+")) == NULL)) {	/* failure, bail out	*/
        return (pop_msg(p,POP_FAILURE,
            "Unable to create temporary message file \"%s\", errno = %d",
                temp_xmit, errno));
    }

    /*  Tell the client to start sending the message */
    pop_msg(p,POP_SUCCESS,"Start sending the message.");

    /*  Receive the message */
#ifdef DEBUG
    if(p->debug)pop_log(p,POP_DEBUG,"Receiving mail message");
#endif
    while (fgets(buffer,MAXLINELEN,p->input)){
        /*  Look for initial period */
#ifdef DEBUG
        if(p->debug)pop_log(p,POP_DEBUG,"Receiving: \"%s\"",buffer);
#endif
        if (*buffer == '.') {
            /*  Exit on end of message */
            if (strcmp(buffer,".\r\n") == 0)
	         break;
	    /* sendmail will not remove escaped .. */
	    else if (buffer[1] == '.') {
		(void)fputs (&buffer[1], tmp);
	    } else {
		(void)fputs (buffer, tmp);
	    }
        } else
	    (void)fputs (buffer, tmp);
    }
    (void)fclose (tmp);

#ifdef DEBUG
    if(p->debug)pop_log(p,POP_DEBUG,"Forking for \"%s\"",MAIL_COMMAND);
#endif
    /*  Send the message */
    switch (pid = fork()) {
        case 0:
	    /*  Open the log file */
	    (void)closelog();
#ifdef SYSLOG42
	    (void)openlog(p->myname,0);
#else
	    (void)openlog(p->myname,LOG_PID,POP_FACILITY);
#endif
	    pop_log(p, POP_DEBUG,
		    "Pop transmit from \"%s\" on \"%s\"", p->user, p->client);

            (void)fclose (p->input);
            (void)fclose (p->output);       
            (void)close(0);
            if (open(temp_xmit,O_RDONLY,0) < 0)
		(void)_exit(1);
            (void)execl (MAIL_COMMAND,"send-mail","-t","-oem",NULLCP);
            (void)_exit(1);
        case -1:
            if (!p->debug) (void)unlink (temp_xmit);
            return (pop_msg(p,POP_FAILURE, "Unable to execute \"%s\" (%d)",
							MAIL_COMMAND, errno));
        default:

#ifdef NEXT
            while((id = wait(&stat)) >=0 && id != pid);
#else
            id = waitpid(pid, &stat, 0);
#endif
            if (!p->debug) (void)unlink (temp_xmit);

#ifdef NEXT
            if (!WIFEXITED (stat))
#else
            if ((!WIFEXITED (stat)) || (WEXITSTATUS (stat) != 0))
#endif
                return (pop_msg(p,POP_FAILURE,"Unable to send message"));

            return (pop_msg (p,POP_SUCCESS,"Message sent successfully"));
    }
}

