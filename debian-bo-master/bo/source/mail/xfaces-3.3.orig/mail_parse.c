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
 * Last Modified On: Thu Jan 20 11:11:36 1994
 * Update Count    : 8
 * Status          : Released
 * 
 * HISTORY
 * 20-Jan-1994		Chris Liebman	
 *    Last Modified: Tue Jan 18 17:37:14 1994 #7 (Chris Liebman)
 *    Now only parse addresses here.  Header and mailbox parsing is now
 *    done in other files.
 *
 * 14-Jan-1994		Chris Liebman	
 *    Last Modified: Fri Jan 14 10:09:52 1994 #3 (Chris Liebman)
 *    Added support for teh status header.
 *    Improved mail parsing, now detects when we are in the body and
 *    will not match headers there!
 * 
 * PURPOSE
 * 	Routines to parse the mail file and to parse a mail address.
*/

#ifndef lint
static char *RCSid = "$Id: mail_parse.c,v 1.7 1994/03/07 20:30:49 liebman Exp $";
#endif

#include "faces.h"

void
MailParseAddress(from, user, host)
String	from;
String	*user;
String	*host;
{
    String	phost;
    String	puser;
    String	data;
    String	data1;
    
    data1 = data = XtNewString(from);
    
    /*
     *    Make lower case.
    */
    
    for(phost = data; *phost != '\0'; ++phost)
    {
	if (isupper(*phost))
	{
	    *phost = tolower(*phost);
	}
    }
    
    /*
     *    space out any comment.
    */
    
    while ((phost = index(data, '(')) != NULL)
    {
	while((*phost != ')') && (*phost != '\0'))
	{
	    *phost++ = ' ';
	}
	
	if (*phost == ')')
	{
	    *phost = ' ';
	}
    }
    
    /*
     *   Skip any leading spaces.
    */
    
    while(isspace(*data))
    {
	++data;
    }
    
    /*
     * See if there is an address in <>'s.
    */
    
    if ((phost = index(data, '<')) != NULL)
    {
	data = phost+1;
    }
    
    /*
     * drop any junk off the end.
    */
    
    for (phost = data; *phost != '\0'; ++phost)
    {
	if (isspace(*phost) || (*phost == ',') || (*phost == '>'))
	{
	    *phost =  '\0';
	    break;
	}
    }

    /*
     * Skip past any leading '@'.
    */

    if (*data == '@')
    {
	++data;
    }
    
    /*
     *   Look for user@host first.
    */
    
    if ((phost = index(data, '@')) != NULL)
    {
	*(phost++) = '\0';
	
	/*
	 *   Look for host!user
	*/
	
	if ((puser = rindex(data, '!')) != NULL)
	{
	    /*
	     *   Yep!, wipe the ! and point to the user name.
	    */
	    
	    *(puser++) = '\0';
	    
	    /*
	     *    Now locate the host name.
	    */
	    
	    if ((phost = rindex(data, '!')) != NULL)
	    {
		phost++;
	    }
	    else
	    {
		phost = data;
	    }
	    
	    /*
	     *    Return the user and host.
	    */
	    
	    *host = XtNewString(phost);
	    *user = XtNewString(puser);
	    XtFree(data1);
	    return;
	}
	
	/*
	 * Grab the user skipping past any RFC822 routing garbage.
	*/
	
	if ((puser = index(data, ':')) != NULL)
	{
	    ++puser;
	}
	else
	{
	    puser = data;
	}
	
	/*
	 *    Return the user and host.
	*/
	
	*host = XtNewString(phost);
	*user = XtNewString(puser);
	XtFree(data1);
	return;
    }
    
    /*
     *   Ok, try uucp style: host!host!user
    */
    
    if ((puser = rindex(data, '!')) != NULL)
    {
	/*
	 *    Wipe ! and move to user name.
	*/
	
	*(puser++) = '\0';
	
	/*
	 *    Now locate the host name.
	*/
	
	if ((phost = rindex(data, '!')) != NULL)
	{
	    phost++;
	}
	else
	{
	    phost = data;
	}
	
	/*
	 *    Return the user and host.
	*/
	
	*host = XtNewString(phost);
	*user = XtNewString(puser);
	XtFree(data1);
	return;
    }
    
    /*
     *    Must be local address!
    */
    
    *user = XtNewString(data);
    *host = XtNewString("LOCAL");
    XtFree(data1);
    return;
}
