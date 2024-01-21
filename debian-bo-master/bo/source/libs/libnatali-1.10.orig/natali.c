/*
** natali/natali.c
** Copyright 1995, 1996, Trinity College Computing Center.
** Writen by David Chappell.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software and documentation are provided "as is" without
** express or implied warranty.
**
** This file is part of an AppleTalk Library Interface compatible library
** for Netatalk. 
**
** Last modified 19 January 1996.
*/

#include <stdio.h>
#include <sys/types.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "appletalk.h"
#include "nbp.h"
#include "pap.h"

#include "natali.h"

/* These variables are globals which hold error return values
   from the AppleTalk functions.
   */
int nbp_errno = 0;
int pap_errno = 0;

/*
** Print debugging information.
*/
void natali_debug(const char string[], ... )
    {
    static int f = -1;
    static pid_t lastpid = -1;
    static time_t start_time;
    pid_t pid;
    va_list va;
    char line[700];
    
    pid = getpid();
    
    if( pid != lastpid && f != -1 )
    	{
	close(f);
	f = -1;
	}
    lastpid = pid;
	
    if( f == -1 )
    	{
	char fname[256];
    	
	sprintf(fname,"/tmp/natali.debug%ld", (long)pid);

	f = open(fname, O_WRONLY | O_CREAT | O_APPEND, 0644);

	time(&start_time);
	}
    
    sprintf( line, "%ld ", time((time_t*)NULL) - start_time );

    va_start(va, string);
    vsprintf(&line[strlen(line)], string, va);
    va_end(va);
    strcat(line,"\n");

    if( f != -1 ) write(f, line, strlen(line));
    } /* end of natali_debug() */

/*
** Global variables.
*/
void *natali_fds[NATALI_MAXFDS];	/* one for each file descriptor */
int natali_started = FALSE;

/*
** Natali shutdown function.  natali_init() calls atexit()
** to ensure that this will be called at program termination.
**
** This is NOT part of the API, it should not be called directly.
*/
void natali_shutdown(void)
    {
    int x;
    
    DODEBUG(("natali_shutdown()"));

    for(x=0; x < NATALI_MAXFDS; x++)
    	{
    	if(natali_fds[x] != (void*)NULL )
    	    {
	    switch(*(int*)natali_fds[x])
	    	{
		case SIGNITURE_PAP:
		    pap_close(x);
		    break;
	    	}
    	    }
    	}  
    
    } /* end of natali_shutdown() */

/*
** This initialization function.  This is called by functions 
** such as pap_open() if natali_started is not true.
**
** This is NOT part of the API, it should not be called directly.
*/
void natali_init(void)
    {
    int x;

    DODEBUG(("natali_init()"));

    natali_started = TRUE;

    for(x=0; x < NATALI_MAXFDS; x++)
    	natali_fds[x] = (void*)NULL;
    	
    atexit(natali_shutdown);    
    } /* end of natali_init() */

/* end of file */
