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
 * Last Modified On: Sun Feb 20 14:09:37 1994
 * Update Count    : 53
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Routines to handle mail message bodys.
*/

#ifndef lint
static char *RCSid = "$Id: mail_body.c,v 1.2 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"

#define	BUFFER_SIZE	4096
#define	BUFFER_SIZE_INC	4096

static char*	buffer = NULL;
static int	buffer_size = 0;

/*
 *   Read the mail body lines.  The body ends with
 * a line starts with the MailSeperator.  Note: This should be
 * configurable.
*/

char *
MailBodyRead(content_length)
int content_length;
{
    int		ch;
    int		string_length = 0;
    int		get_more;
    char*	str;
    char	sepbuf[MAX_MAILSEP_SIZE+1];

    /*
     * If allowed, use the content length to skip the body.
    */
    
    if (TheFacesResources.use_content_length && content_length >= 0)
    {
	if (content_length == 0)
	{
	    return NULL;
	}

	/*
	 *  Make sure the buffer is big enough.
	*/

	if (buffer_size == 0)
	{
	    buffer_size = content_length + 1;
	    buffer = XtMalloc(buffer_size);
	}
	else if (buffer_size <= content_length)
	{
	    buffer_size = content_length + 1;
	    buffer = XtRealloc(buffer, buffer_size);
	}
	
	MailFileReadString(buffer, buffer_size);

	if (buffer[0] == '\0')
	{
	    return NULL;
	}
	
	/*
	 *  Allocate a buffer that we can return.
	*/
	
	str = XtMalloc(string_length + 1);
	strcpy(str, buffer);
	
	return str;
    }
    
    /*
     * See if this is the start of a new message,
    */
    
    MailFilePeekString(sepbuf, MailSeperatorLength);
    
    if (strcmp(sepbuf, MailSeperator) == 0)
    {
	/*
	 *  If we do not want the seperator then skip it.
	*/
	
	if (MailSeperatorSkip)
	{
	    MailFileSkipString(MailSeperatorLength);
	}
	
	/*
	 * Null message body.
	*/
	
	return NULL;
    }
    
    /*
     *  Read  the first char. Check for end of file.
    */
    
    ch = MailFileReadChar();
    
    if (ch == EOF)
    {
	return NULL;
    }
    
    /*
     * If the buffer has not beed created yet then create it.
    */
    
    if (buffer_size == 0)
    {
	buffer = XtMalloc(BUFFER_SIZE);
	buffer_size = BUFFER_SIZE;
    }
    
    /*
     * Ok, start collecting characters.
    */
    
    do
    {
	get_more = 1;
	
	/*
	 * Collect the rest of this line.
	*/
	
	while((ch != '\n') && (ch != EOF))
	{
	    if (string_length >= buffer_size)
	    {
		/*
		 * Grow buffer.
		*/
		
		buffer = XtRealloc(buffer, buffer_size + BUFFER_SIZE_INC);
		buffer_size += BUFFER_SIZE_INC;
	    }
	    
	    buffer[string_length++] = ch;
	    
	    ch = MailFileReadChar();
	}

	if (ch == EOF)
	{
	    get_more = 0;
	}
	else
	{
	    /*
	     * Ok, We have gotten to the end of a line.  Peek at the start
	     * of the next line to see if it is the seperator for the
	     * next message.
	    */
	    
	    MailFilePeekString(sepbuf, MailSeperatorLength);
	    
	    if (strcmp(sepbuf, MailSeperator) == 0)
	    {
		/*
		 *  If we do not want the seperator then skip it.
		*/
		
		if (MailSeperatorSkip)
		{
		    MailFileSkipString(MailSeperatorLength);
		}
		
		get_more = 0;
	    }
	}
    } while (get_more);
    
    /*
     * Terminate the string.
    */
    
    buffer[string_length] = '\0';
    
    /*
     *  Allocate a buffer that we can return.
    */
    
    str = XtMalloc(string_length + 1);
    strcpy(str, buffer);
    
    return (str);
}

/*
 *   Skip the mail body lines.  The body ends with
 * a line starts with the MailSeperator.  Note: This should be
 * configurable.
*/

void
MailBodySkip(content_length)
int content_length;
{
    int		ch;
    int		get_more;
    char	sepbuf[MAX_MAILSEP_SIZE];
    
    /*
     * If allowed, use the content length to skip the body.
    */
    
    if (TheFacesResources.use_content_length && content_length >= 0)
    {
	if (content_length == 0)
	{
	    return;
	}
	
	MailFileSkipString(content_length);
	return;
    }
    
    /*
     * See if this is the start of a new message,
    */
    
    MailFilePeekString(sepbuf, MailSeperatorLength);
    
    if (strcmp(sepbuf, MailSeperator) == 0)
    {
	/*
	 *  If we do not want the seperator then skip it.
	*/
	
	if (MailSeperatorSkip)
	{
	    MailFileSkipString(MailSeperatorLength);
	}
	
	/*
	 * Null message body.
	*/
	
	return;
    }
    
    /*
     *  Read  the first char. Check for end of file.
    */
    
    ch = MailFileReadChar();
    
    if (ch == EOF)
    {
	return;
    }
    
    /*
     * Ok, start collecting characters.
    */
    
    do
    {
	get_more = 1;
	
	/*
	 * Collect the rest of this line.
	*/
	
	while((ch != '\n') && (ch != EOF))
	{
	    ch = MailFileReadChar();
	}
	
	if (ch == EOF)
	{
	    get_more = 0;
	}
	else
	{
	    /*
	     * Ok, We have gotten to the end of a line.  Peek at the start
	     * of the next line to see if it is the MailSeperator for the
	     * next message.
	    */
	    
	    MailFilePeekString(sepbuf, MailSeperatorLength);
	    
	    if (strcmp(sepbuf, MailSeperator) == 0)
	    {
		/*
		 *  If we do not want the seperator then skip it.
		*/
		
		if (MailSeperatorSkip)
		{
		    MailFileSkipString(MailSeperatorLength);
		}
		
		get_more = 0;
	    }
	}
    } while (get_more && (ch = MailFileReadChar()) != EOF);
}

