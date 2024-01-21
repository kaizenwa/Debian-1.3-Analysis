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
 * Created On      : Sat Jan 29 15:17:46 1994
 * Last Modified By: Chris Liebman
 * Last Modified On: Sat Feb 19 21:56:14 1994
 * Update Count    : 49
 * Status          : Released
 * 
 * HISTORY
 * 13-Feb-1994		Chris Liebman	
 *    Last Modified: Sat Feb 12 23:28:52 1994 #42 (Chris Liebman)
 *    Added FormatCommand support.
 *
 * 
 * PURPOSE
 * 	Search path for files named user@host.
*/

#ifndef lint
static char *RCSid = "$Id: face_search_uh.c,v 1.2 1994/03/07 20:26:45 liebman Exp $";
#endif

#include "faces.h"
#include "face_search.h"

static int
FaceSearchUserHost(item, data)
MailItem*	item;
FaceSearchData*	data;
{
    char*	name;
    int		reallen = 0;
    
    if (item->realhost != NULL)
    {
	reallen = strlen(item->realhost);
    }
    
    /*
     *    Make a buffer for UserHost name construction.
    */
    
    name = XtMalloc(strlen(item->user)   + strlen(item->host) + reallen + 9);
    
    /*
     *    Build a User@Host name for this sucker.
    */
    
    sprintf(name, "%s@%s", item->user, item->host);
    
    /*
     *    Try to load the thing in.
    */
    
    if (FaceSearchLoad(name, item, data))
    {
	XtFree(name);
	return 1;
    }
    
    /*
     * Try the real (looked up) host if available.
    */
    
    if (item->realhost != NULL)
    {
	sprintf(name, "%s@%s", item->user, item->realhost);
	
	/*
	 *    Try to load the thing in.
	*/
	
	if (FaceSearchLoad(name, item, data))
	{
	    XtFree(name);
	    return 1;
	}
    }
    
    XtFree(name);
    
    /*
     *   Oh well.  Lets try for just the user name.
    */
    
    if (FaceSearchLoad(item->user, item, data))
    {
	return 1;
    }
    
    /*
     *   Oh well.  Lets try for just the host name.
    */
    
    if (FaceSearchLoad(item->host, item, data))
    {
	return 1;
    }

    /*
     * Try the real (looked up) hostname if available.
    */

    if (item->realhost != NULL && FaceSearchLoad(item->realhost, item, data))
    {
	return 1;
    }
    
    /*
     *   Oh well...
    */
    
    return(0);
}

static FaceSearchType UserHost =
{
    "u@h",
    FaceSearchUserHost,
    NULL,
    NULL,
};

void
FaceSearchUserHostInit()
{
    FaceSearchTypeRegister(&UserHost);
}
