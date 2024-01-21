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
 * Last Modified On: Sun Feb 20 14:13:33 1994
 * Update Count    : 21
 * Status          : Released
 * 
 * HISTORY
 * 
 * PURPOSE
 * 	Routines to parse the mail file.
*/

#ifndef lint
static char *RCSid = "$Id: mail_box.c,v 1.2 1994/02/23 13:17:02 liebman Exp $";
#endif

#include "faces.h"

char*		MailSeperator = NULL;
int   		MailSeperatorLength = 0;
Boolean		MailSeperatorSkip = False;
static Boolean	MailSeperatorKnown = False;

/*
 * There is mail and the mailbox has been changed.
*/

void
MailBoxParse()
{
    MailHeader*	headers;
    MailHeader*	header;
    int content_length;
    char	sepbuf[MAX_MAILSEP_SIZE+1];
    
    /*
     * Open the mail spool file.  If we fail then we act as
     * though the mail box is empty.
    */
    
    if (!MailFileOpen(TheFacesResources.spool_file))
    {
	MailBoxEmpty();
	return;
    }
    
    /*
     *  If we do not know what the mail seperator is then try to guess.
    */
    
    if (!MailSeperatorKnown)
    {
	/*
	 *    Peek at the first few chars and see what kind of mail seperator
	 * is in use.
	*/
	
	MailFilePeekString(sepbuf, MAX_MAILSEP_SIZE);
	
	if (strncmp(sepbuf, MAIL_SEP1, strlen(MAIL_SEP1)) == 0)
	{
	    MailSeperator       = MAIL_SEP1;
	    MailSeperatorLength = strlen(MAIL_SEP1);
	    MailSeperatorSkip	= MAIL_SEP1_SKIP;
	    MailSeperatorKnown  = True;
	}
	else if (strncmp(sepbuf, MAIL_SEP2, strlen(MAIL_SEP2)) == 0)
	{
	    MailSeperator       = MAIL_SEP2;
	    MailSeperatorLength = strlen(MAIL_SEP2);
	    MailSeperatorSkip	= MAIL_SEP2_SKIP;
	    MailSeperatorKnown  = True;
	}
	else
	{
	    /*
	     * Just assume MAIL_SEP1 untill we know for sure.
	    */
	    
	    MailSeperator       = MAIL_SEP1;
	    MailSeperatorLength = strlen(MAIL_SEP1);
	    MailSeperatorSkip	= MAIL_SEP1_SKIP;
	}
    }
    
    /*
     *  If we need to skip the seperator then skip it.
    */

    if (MailSeperatorSkip)
    {
	MailFilePeekString(sepbuf, MailSeperatorLength);
	
	if (strcmp(sepbuf, MailSeperator) == 0)
	{
	    MailFileSkipString(MailSeperatorLength);
	}
    }
    
    /*
     *   Read each message from the mail file.
    */
    
    while((headers = MailHeaderListRead()) != NULL)
    {
	content_length = -1;
	
	if (TheFacesResources.use_content_length &&
	    ((header = MailHeaderFind("Content-Length:", headers)) != NULL))
	{
	    content_length = atoi(header->value);
	}
	
	/*
	 *   We got a list of headers so skip the body.
	*/
	
	MailBodySkip(content_length);

	/*
	 *  If we used the content_length value then we should skip
	 * at most one leftover blank line. (I am nor sure that this
	 * is nessarry as my mailer users the From_ header and messages
	 * with the content-length header had an extra blank line).
	*/

	if (TheFacesResources.use_content_length &&
	    MailFilePeekChar() == '\n')
	{
	    (void) MailFileReadChar();
	}
	
	/*
	 *   Create a mail item.
	*/
	
	MailItemCreate(headers);
    }
    
    MailFileClose();
    
    return;
}
