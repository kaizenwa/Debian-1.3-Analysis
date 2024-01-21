/*                             -*- Mode: C++-C -*- 
 * 
 *  
 * 	     Copyright 1994 Christopher B. Liebman
 * 
 *  Permission to use, copy, modify, distribute, and sell this software
 *  and its documentation for any purpose is hereby granted without fee,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name Christopher B. Liebman not
 *  be used in advertising or publicity pertaining to distribution of this
 *  software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  CHRISTOPHER B. LIEBMAN, DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL CHRISTOPHER
 * B. LIEBMAN, BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING SPECIAL,
 * INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA, OR
 * PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * 
 * 
 * Author          : Chris Liebman
 * Created On      : Sat Feb  5 20:08:55 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sat Feb  5 22:14:20 1994
 * Update Count    : 39
 * Status          : Released.
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Support for reading POP mailboxes.
*/

#ifndef lint
static char* RCSid = "$Id: pop_check.c,v 1.5 1994/02/13 22:33:30 liebman Exp $";
#endif

#include "faces.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

FILE*	input = NULL;
FILE*	output = NULL;

static char* buffer = NULL;
static int buffer_size = 0;

static int
PopReadLine(stripcr)
int	stripcr;
{
    int	c;
    int	len = 0;
    
    if (buffer == NULL)
    {
	buffer_size = 1024;
	buffer = XtMalloc(buffer_size);
    }
    
    while((c = fgetc(input)) != EOF)
    {
	if (len >= (buffer_size - 1))
	{
	    buffer_size += 1024;
	    buffer = XtRealloc(buffer, buffer_size);
	}
	
	/*
	 *  Dont save CR if asked.
	*/
	
	if (!stripcr || (c != '\r'))
	{
	    buffer[len++] = c;
	}
	
	if (c == '\n')
	{
	    break;
	}
    }
    
    buffer[len] = '\0';

#ifdef POP_DEBUG
    fprintf(stderr, "PopReadLine:%d:%s", len, buffer);
#endif
    return len;
}

static int
PopResponce()
{
    /*
     * Flush any putput.
    */
    
    fflush(output);
    /*
     *   Get the responce.
    */
    
    if (!PopReadLine(0))
    {
	return 0;
    }

    if (buffer[0] != '+')
    {
	return 0;
    }

    return 1;
}

static int
PopCreate(hostname)
char*	hostname;
{
    int			fd1, fd2;
    struct sockaddr_in	sa ;
    struct hostent*	hp;
    long		addr;
    
#ifdef POP_DEBUG
    fprintf(stderr, "PopConnect: %s\n", hostname);
#endif
    
    bzero(&sa, sizeof(sa));
    
    /*
     *   This will allow 192.xx.x.xxx format for the hostname.
    */
    
    if ((addr = inet_addr(hostname)) != -1)
    {
	/*
	 * Is Internet addr in octet notation
	*/
	
	bcopy(&addr, (char *) &sa.sin_addr, sizeof(addr));
	sa.sin_family = AF_INET ;
    }
    else
    {
	/*
	 *  Lookup the host.
	*/
	
	if ((hp = gethostbyname(hostname)) == NULL)
	{
	    fprintf(stderr, "unknown hosts: %s\n", hostname);
	    return 0;
	}
	
	bcopy(hp->h_addr, (char *) &sa.sin_addr, hp->h_length);
	sa.sin_family = hp->h_addrtype;
    }
    
    sa.sin_port = htons(TheFacesResources.pop_port);
    
    fd1 = socket(sa.sin_family, SOCK_STREAM, 0);
    
    if (fd1 < 0)
    {
	/*
	 * Failed to create socket.
	*/
	
	perror("socket:");
	return 0;
    }
    
    if (connect(fd1, (struct sockaddr *)&sa, sizeof(sa)) < 0)
    {
	/*
	 * Failed to connect.
	*/
	
	perror("connect:");
	close(fd1);
	return 0;
    }

    /*
     *  Make a second file descripter.
    */
    
    fd2 = dup(fd1);

    if (fd2 < 0)
    {
	perror("dup:");
	close(fd1);
	return 0;
    }

    /*
     *  Make stdio files from these things.
    */

    input = fdopen(fd1, "r");
    if (input == NULL)
    {
	close(fd1);
	close(fd2);
	fprintf(stderr, "fdopen failed!\n");
	return 0;
    }
    
    output = fdopen(fd2, "w");
    if (output == NULL)
    {
	fclose(input);
	close(fd2);
	fprintf(stderr, "fdopen failed!\n");
	return 0;
    }

    if (!PopResponce())
    {
	fclose(input);
	fclose(output);
	fprintf(stderr, "failed to read greeting!\n");
	return 0;
    }
    
    return 1;
}

static int
PopQuit()
{
#ifdef POP_DEBUG
    fprintf(stderr, "PopQuit()\n");
#endif
    fprintf(output, "QUIT\r\n");
    
    if (!PopResponce())
    {
	fprintf(stderr, "PopQuit: failed: <%s>\n", buffer);
    }
    
    fclose(input);
    fclose(output);
    
    return 1;
}

    
static int
PopStat(nmsgs, nbytes)
int*	nmsgs;
int*	nbytes;
{
    *nmsgs  = 0;
    *nbytes = 0;
    
#ifdef POP_DEBUG
    fprintf(stderr, "PopStat()\n");
#endif
    fprintf(output, "STAT\r\n");

    if (!PopResponce())
    {
	return 0;
    }
    
    sscanf(buffer, "+OK %d %d", nmsgs, nbytes);
    
    return 1;
}

static int
PopUser(user)
char*	user;
{
#ifdef POP_DEBUG
    fprintf(stderr, "PopUser(%s)\n", user);
#endif
    fprintf(output, "USER %s\r\n", user);
    
    if (!PopResponce())
    {
	return 0;
    }
    
    return 1;
}

static int
PopPass(pass)
char*	pass;
{
#ifdef POP_DEBUG
    fprintf(stderr, "PopPass(%s)\n", pass);
#endif
    fprintf(output, "PASS %s\r\n", pass);

    if (!PopResponce())
    {
	return 0;
    }
    
    return 1;
}


static int
PopList(item, size)
int item;
int *size;
{
    int ret_item;
    
#ifdef POP_DEBUG
    fprintf(stderr, "PopList(%d)\n", item);
#endif
    fprintf(output, "LIST %d\r\n", item);
    
    if (!PopResponce())
    {
	return 0;
    }

    sscanf(buffer, "+OK %d %d", &ret_item, size);
    
    if (item != ret_item)
    {
	return 0;
    }
    
    return 1;
}

static char*
PopRetr(item)
int item;
{
    int msg_size;
    int got = 0;
    int	len;
    char* msg;
    
    if (!PopList(item, &msg_size))
    {
	return NULL;
    }
    
    msg = XtMalloc(msg_size+1);
    msg[0] = '\0';
    
#ifdef POP_DEBUG
    fprintf(stderr, "PopRetr(%d)\n", item);
#endif
    
    fprintf(output, "RETR %d\r\n", item);
    
    if (!PopResponce())
    {
	return NULL;
    }
    
    for(len = PopReadLine(1);
	strcmp(buffer, ".\n") != 0;
	len = PopReadLine(1))
    {
	strcpy(&msg[got], buffer);
	got += len;
    }
    
#ifdef POP_DEBUG
    fprintf(stderr, "got: %d\n", got);
#endif
    
    return msg;
}

int
PopAuthInfo(user, pass)
char** user;
char** pass;
{
    FILE*	auth;
    FILE*	saved_in;
    int		len;
    char**	data;
    
    auth = fopen(TheFacesResources.pop_auth_file, "r");
    
    if (auth == NULL)
    {
	return 0;
    }
    
    saved_in = input;
    input = auth;
    
    len = PopReadLine();
    
    input = saved_in;
    fclose(auth);
    
    data = StringParse(buffer, " \t\n");
    
    if (!data[0] || !data[1] || data[2])
    {
	bzero(buffer, buffer_size);
	XtFree((char*)data);
	return 0;
    }
    
    *user = XtNewString(data[0]);
    *pass = XtNewString(data[1]);
    bzero(buffer, buffer_size);
    XtFree((char*)data);
    
    return 1;
}

void
PopCheck()
{
    MailHeader*	headers;
    char*	user;
    char*	pass;
    int		mbox_count;
    int		mbox_size;
    int		i;
    
    /*
     *   Read in the pop login and passwd.
    */

    if (!PopAuthInfo(&user, &pass))
    {
	return;
    }
    
    if (!PopCreate(TheFacesResources.pop_host))
    {
	bzero(user, strlen(user));
	bzero(pass, strlen(pass));
	XtFree(user);
	XtFree(pass);
	return;
    }
    
    if (!PopUser(user))
    {
	bzero(user, strlen(user));
	bzero(pass, strlen(pass));
	XtFree(user);
	XtFree(pass);
	PopQuit();
	return;
    }
    
    if (!PopPass(pass))
    {
	bzero(user, strlen(user));
	bzero(pass, strlen(pass));
	XtFree(user);
	XtFree(pass);
	PopQuit();
	return;
    }
    
    bzero(user, strlen(user));
    bzero(pass, strlen(pass));
    XtFree(user);
    XtFree(pass);
    
    if (!PopStat(&mbox_count, &mbox_size))
    {
	PopQuit();
	return;
    }
    
    for(i = 0; i < mbox_count; ++i)
    {
	char* msg;
	
	msg = PopRetr(i+1);
	
	if (!msg)
	{
	    PopQuit();
	    return;
	}

	/*
	 * Stuff the message as unread mail data.
	*/
	
	MailFileUnReadString(msg);

	/*
	 * Parse headers!
	*/

	headers = MailHeaderListRead();
	
	/*
	 *  Create item.
	*/
	
	MailItemCreate(headers);
	
	XtFree(msg);
    }
    
    PopQuit();
}
