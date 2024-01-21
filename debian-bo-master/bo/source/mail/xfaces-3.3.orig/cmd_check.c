/*                             -*- Mode: C++-C -*- 
 * 
 *		 Copyright 1994 Christopher B. Liebman
 *
 *     Permission to use, copy, modify, distribute, and sell this software
 *     and its documentation for any purpose is hereby granted without fee,
 *     provided that the above copyright notice appear in all copies and that
 *     both that copyright notice and this permission notice appear in
 *     supporting documentation, and that the name Christopher B. Liebman not
 *     be used in advertising or publicity pertaining to distribution of this
 *     software without specific, written prior permission.
 *
 *    THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 *    ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 *    LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 *    PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 *    B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 *    INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 *    PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 *    WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 *    OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author          : Chris Liebman
 * Created On      : Tue Jan 11 14:11:30 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Fri Feb  4 08:20:34 1994
 * Update Count    : 3
 * Status          : Released
 * 
 * HISTORY
 * 4-Feb-1994		Chris Liebman	
 *    Last Modified: Tue Jan 11 14:29:22 1994 #1 (Chris Liebman)
 *    Added code to handle the Cols=nnn Rows=nnn line.
 * 
 * PURPOSE
 * 	Use listCommand to get new list.
*/

#ifndef lint
static char *RCSid = "$Id: cmd_check.c,v 1.3 1994/02/04 13:20:43 liebman Exp $";
#endif

#include "faces.h"
#include <sys/stat.h>

static char*	buffer = NULL;
static int	buffer_size = 0;

static int
CmdReadLine(cmd)
FILE* cmd;
{
    int	len;
    int c;
    
    if (buffer == NULL)
    {
	buffer_size = 1024;
	buffer = XtMalloc(buffer_size);
    }
    
    len = 0;
    buffer[len] = '\0';
    
    while((c = fgetc(cmd)) != EOF && c != '\n')
    {
	if (len >= (buffer_size - 1))
	{
	    buffer_size += 1024;
	    buffer = XtRealloc(buffer, buffer_size);
	}
	
	buffer[len++] = c;
	buffer[len] = '\0';
    }
    
    /*
     *   We only return 0 at eof.
    */
    
    if (len == 0 && c != EOF)
    {
	buffer[len++] = '\n';
	buffer[len] = '\0';
    }
    
    return len;
}

void
CmdCheck()
{
    FILE*	cmd;
    char**	fields;
    int		cols = 0;
    int		rows = 0;
    
    cmd = popen(TheFacesResources.list_command, "r");
    
    if (cmd == NULL)
    {
	return;
    }
    
    /*
     *    read the first two lines.
     * (We currently ignore these lines, they
     * look like: "host\tuser" and "Cols=mm Rows=nn".
    */
    
    CmdReadLine(cmd);

    /*
     *   Read the number of rows and columns.
    */
    
    if (CmdReadLine(cmd))
    {
	fields = StringParse(buffer, "= \t\n");
	
	/*
	 *  Expect four fields to of which should be fixed strings.
	*/
	
	if (fields[0] && fields[1] && fields[2] && fields[3] && !fields[4] &&
	    (strcmp(fields[0], "Cols") == 0) &&
	    (strcmp(fields[2], "Rows") == 0))
	{
	    cols = atoi(fields[1]);
	    rows = atoi(fields[3]);
	    
	    XtVaSetValues(TheFrame,
			  XtNsetWidth,  cols,
			  XtNsetHeight, rows,
			  NULL);
	}
	else
	{
	    fprintf(stderr, "malformed Cols=nnn Rows=nnn line.\n");
	}
	
	XtFree((char*) fields);
    }
    
    while(CmdReadLine(cmd))
    {
	fields = StringParse(buffer, "\t\n");
	
	/*
	 *  The first two fields are required!
	*/
	
	if (!fields[0] || !fields[1])
	{
	    continue;
	}
	
	MailItemCreateNoHeaders(fields[0], fields[1], &fields[2]);
	XtFree((char*) fields);
    }
    
    pclose(cmd);
    
    return;
}

