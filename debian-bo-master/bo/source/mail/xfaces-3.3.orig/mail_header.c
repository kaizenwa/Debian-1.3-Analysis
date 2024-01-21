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
 * Last Modified On: Sun Feb 20 13:01:44 1994
 * Update Count    : 38
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Routines to handle mail headers.
*/

#ifndef lint
static char *RCSid = "$Id: mail_header.c,v 1.2 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"

#define	BUFFER_SIZE	80
#define	BUFFER_SIZE_INC	80

static char*	buffer = NULL;
static int	buffer_size = 0;

/*
 *   Read a mail header line from given file.  Handle line continuations.
 * If the first char is a newln then there are no more headers.
*/

char*
MailHeaderLineRead()
{
    int		ch;
    int		string_length = 0;
    int		get_more;
    char*	str;
    
    /*
     *  Look at the first char.  If it is a newln then there are no more
     * headers and the newln is pushed back.  Also return NULL if end of
     * file.
    */
    
    ch = MailFileReadChar();
    
    if (ch == EOF)
    {
	return NULL;
    }
    
    if (ch == '\n')
    {
	MailFileUnReadChar(ch);
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
	get_more = 0;
	
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
	
	/*
	 * Ok, We have gotten to the end of a line.  If the first character
	 * on the next line is a tab then it is a continuation of this
	 * line and we need to skip the tab and continue reading chars.
	*/
	
	if (ch != EOF)
	{
	    /*
	     * Get the next character, and if it is a TAB then we continue
	     * the loop.
	    */
	    
	    ch = MailFileReadChar();
	    if ((ch == '\t') || (ch == ' '))
	    {
		get_more = 1;
	    }
	    else
	    {
		/*
		 * Not a line continuation, put back.
		*/
		
		MailFileUnReadChar(ch);
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
 * Read a header line and create a header struct.
*/

MailHeader*
MailHeaderRead()
{
    MailHeader*	header;
    char*	line;
    char*	p;
    int		len;
    
    /*
     *   First read a line.
    */
    
    line = MailHeaderLineRead();
    
    /*
     *  No line means no more headers.
    */
    
    if (!line)
    {
	return NULL;
    }
    
#ifdef DEBUG
    fprintf(stderr, "read header: <%s>\n", line);
#endif
    
    /*
     *  Allocate the header.
    */
    
    header = (MailHeader*) XtMalloc(sizeof(MailHeader));
    header->name  = NULL;
    header->value = NULL;
    header->line  = NULL;
    header->next  = NULL;
    header->prev  = NULL;
    
    /*
     *   Now locate the try to parse out the line.  First skip to the:
     * at the end of this header name (note that we accept a ':' *or*
     * a ` ` as the terminator.
    */
    
    for (p = line; *p && *p != ':' && *p != ' '; ++p);
    
    if (!*p)
    {
	/*
	 * Bogus header!  We failed to parse it.
	*/
	
	header->line = line;
#ifdef DEBUG
	fprintf(stderr, "bogus header: <%s>\n", line);
#endif
	return header;
    }
    
    ++p;
    
    /*
     *  Allocate the name and value.
    */
    
    len = p - line;
    header->name = XtMalloc(len + 1);
    strncpy(header->name, line, len);
    header->name[len] = '\0';
    
    header->value = XtMalloc(strlen(p)+1);
    strcpy(header->value, p);
    
    XtFree(line);
    
#ifdef DEBUG
    fprintf(stderr, "parsed header: <%s> <%s>\n", header->name, header->value);
#endif    
    return header;
}

/*
 *   Read in all of the mail headers for a mail messages.
*/

MailHeader*
MailHeaderListRead()
{
    MailHeader	*header_head = NULL;
    MailHeader	*header_tail = NULL;
    MailHeader	*header;
    
#ifdef DEBUG
    fprintf(stderr, "reading mail headers...\n");
#endif
    
    while((header = MailHeaderRead()) != NULL)
    {
	/*
	 * We go in the list at the end so there is no next guy.
	*/
	
	header->next = NULL;
	
	/*
	 *  The last guy on the list is our prev.
	*/
	
	header->prev = header_tail;
	
	/*
	 *   If there is a prev guy ne are its next.
	*/
	
	if (header->prev != NULL)
	{
	    header->prev->next = header;
	}
	
	/*
	 *  If there is no head, we are it. This is the
	 * first header.
	*/
	
	if (header_head == NULL)
	{
	    header_head = header;
	}
	
	/*
	 *   We are the tail.
	*/
	
	header_tail = header;
    }

    return header_head;
}

void
MailHeaderFree(header)
MailHeader* header;
{
    if (header->name != NULL)
    {
	XtFree(header->name);
    }

    if (header->value != NULL)
    {
	XtFree(header->value);
    }

    if (header->line)
    {
	XtFree(header->line);
    }
    
    XtFree((void *)header);
}

void
MailHeaderListFree(list)
MailHeader*	list;
{
    MailHeader*	header;
    
    while(list != NULL)
    {
	header = list;
	list = list->next;
	MailHeaderFree(header);
    }
}

#define	MKLOWER(c)	isupper(c) ? tolower(c) : c

static int
HeaderNameCompare(s1, s2)
char*	s1;
char*	s2;
{
    int	c1, c2;
    
    while ((*s1 != '\0') &&
	   (*s2 != '\0'))
    {
	c1 = isupper(*s1) ? tolower(*s1) : *s1;
	c2 = isupper(*s2) ? tolower(*s2) : *s2;
	
	if (c1 != c2)
	{
	    break;
	}
	
	++s1;
	++s2;
    }
    
    return *s1 - *s2;
}


/*
 * Locate the named header in the header list.
*/

MailHeader*
MailHeaderFind(name, list)
char*		name;
MailHeader*	list;
{
    MailHeader	*header;
    
    for(header = list; header != NULL; header = header->next)
    {
	if (header->name != NULL && HeaderNameCompare(name, header->name) == 0)
	{
	    return header;
	}
    }
    
    return NULL;
}

/*
 * Compare two mail header lists.
*/

int
MailHeaderListCompare(list1, list2)
MailHeader*	list1;
MailHeader*	list2;
{
    MailHeader* header1;
    MailHeader*	header2;

    for(header1 = list1, header2 = list2;
	header1 != NULL && header2 != NULL;
	header1 = header1->next, header2 = header2->next)
    {
	int	name1, name2;
	int	value1, value2;
	int	line1, line2;
	
	name1 = (header1->name != NULL);
	name2 = (header2->name != NULL);
	value1 = (header1->value != NULL);
	value2 = (header2->value != NULL);
	line1 = (header1->line != NULL);
	line2 = (header2->line != NULL);

	/*
	 *   Messages must both have or not have names/values/lines.
	*/

	if ((name1  != name2)  ||
	    (value1 != value2) ||
	    (line1  != line2))
	{
	    break;
	}
	
	if (name1  && (strcmp(header1->name,  header2->name)  != 0))
	{
	    break;
	}
	
	if (value1 && (strcmp(header1->value, header2->value) != 0))
	{
	    break;
	}
	
	if (line1 &&  (strcmp(header1->line,  header2->line)  != 0))
	{
	    break;
	}
    }
    
    /*
     * If we got thru all of the headers and both lists are exausted then
     * the header lists are identicle.
    */
    
    if (header1 == NULL && header2 == NULL)
    {
	return 1;
    }
    
    /*
     *   Header lists are different.
    */
    
    return 0;
}

