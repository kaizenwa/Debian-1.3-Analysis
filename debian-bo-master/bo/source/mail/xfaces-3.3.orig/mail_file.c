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
 * Last Modified On: Sun Feb 20 13:54:23 1994
 * Update Count    : 48
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Routines to handle reading of characters from the mail box file.
 * We need to be able to push back single chars and sometimes strings.
*/

#ifndef lint
static char *RCSid = "$Id: mail_file.c,v 1.4 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"

#define	BUFFER_SIZE	100
#define	BUFFER_SIZE_INC	20

static char*	buffer = NULL;
static int	buffer_size = 0;
static int	count;
static FILE*	file;

int
MailFileOpen(name)
char *name;
{
    file = fopen(name, "r");
    
    if (file == NULL)
    {
	return 0;
    }

    return 1;
}

void
MailFileClose()
{
    (void) fclose(file);
    file = NULL;
    count = 0;
}

int
MailFileReadChar()
{
    int	ch;
    
    if (count)
    {
	ch = buffer[--count];
	ch &= 0xff;
    }
    else
    {
	ch = fgetc(file);
    }

    return ch;
}

int
MailFileReadString(str, len)
char*	str;
int	len;
{
    int got;
    int	ch;
    
    for(got = 0; got < len; ++got)
    {
	ch = MailFileReadChar();
	
	if (ch == EOF)
	{
	    break;
	}
	
	str[got] = ch;
    }
    
    str[got] = '\0';

    return got;
}

int
MailFileSkipString(len)
int	len;
{
    int skiped;
    int	ch;
    
    for(skiped = 0; skiped < len; ++skiped)
    {
	ch = MailFileReadChar();
	
	if (ch == EOF)
	{
	    break;
	}
    }

    return skiped;
}

void
MailFileUnReadChar(ch)
int ch;
{
    if (buffer == NULL)
    {
	buffer = XtMalloc(BUFFER_SIZE);
	buffer_size = BUFFER_SIZE;
    }
    
    if (count >= buffer_size)
    {
	buffer_size += BUFFER_SIZE_INC;
	buffer = XtRealloc(buffer, buffer_size);
    }

    buffer[count++] = ch;
}

void
MailFileUnReadString(str)
char* str;
{
    int len;
    
    for (len = strlen(str); len > 0; --len)
    {
	MailFileUnReadChar(str[len - 1]);
    }
}

void
MailFileClearUnRead()
{
    count = 0;
}

int
MailFilePeekChar()
{
    int ch;
    
    ch = MailFileReadChar();
    if (ch != EOF)
    {
	MailFileUnReadChar(ch);
    }
    
    return ch;
}



void
MailFilePeekString(str, len)
char* str;
int len;
{
    MailFileReadString(str, len);
    MailFileUnReadString(str);
}


